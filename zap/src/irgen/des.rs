use crate::config::{Enum, NumTy, PrimitiveTy, Struct, Ty};
use std::collections::HashMap;

use super::{Expr, Gen, Stmt, Var};

struct Des<'src> {
	checks: bool,
	buf: Vec<Stmt>,
	var_occurrences: &'src mut HashMap<String, usize>,
}

impl Gen for Des<'_> {
	fn push_stmt(&mut self, stmt: Stmt) {
		self.buf.push(stmt);
	}

	fn gen<'a, 'src: 'a, I>(mut self, names: &[String], types: I) -> Vec<Stmt>
	where
		I: Iterator<Item = &'a Ty<'src>>,
	{
		for (ty, name) in types.zip(names) {
			self.push_ty(ty, Var::Name(name.to_string()));
		}

		self.buf
	}

	fn get_var_occurrences(&mut self) -> &mut HashMap<String, usize> {
		self.var_occurrences
	}
}

impl Des<'_> {
	fn push_struct(&mut self, struct_ty: &Struct, into: Var) {
		for (name, ty) in struct_ty.fields.iter() {
			self.push_ty(ty, into.clone().eindex(Expr::Str((*name).into())))
		}
	}

	fn push_enum(&mut self, enum_ty: &Enum, into: Var) {
		match enum_ty {
			Enum::Unit(enumerators) => {
				let numty = NumTy::from_f64(0.0, enumerators.len() as f64 - 1.0);

				let (enum_value_name, enum_value_expr) = self.add_occurrence("enum_value");
				self.push_local(enum_value_name, Some(self.readnumty(numty)));

				for (i, enumerator) in enumerators.iter().enumerate() {
					if i == 0 {
						self.push_stmt(Stmt::If(enum_value_expr.clone().eq((i as f64).into())));
					} else {
						self.push_stmt(Stmt::ElseIf(enum_value_expr.clone().eq((i as f64).into())));
					}
					self.push_assign(into.clone(), Expr::StrOrBool(enumerator.to_string()));
				}

				self.push_stmt(Stmt::Else);
				self.push_stmt(Stmt::Error("Invalid enumerator".into()));
				self.push_stmt(Stmt::End);
			}

			Enum::Tagged { tag, variants } => {
				let numty = NumTy::from_f64(0.0, variants.len() as f64 - 1.0);

				let (enum_value_name, enum_value_expr) = self.add_occurrence("enum_value");
				self.push_local(enum_value_name, Some(self.readnumty(numty)));

				for (i, (name, struct_ty)) in variants.iter().enumerate() {
					if i == 0 {
						self.push_stmt(Stmt::If(enum_value_expr.clone().eq((i as f64).into())));
					} else {
						self.push_stmt(Stmt::ElseIf(enum_value_expr.clone().eq((i as f64).into())));
					}

					self.push_assign(
						into.clone().eindex(Expr::Str((*tag).into())),
						Expr::StrOrBool(name.to_string()),
					);
					self.push_struct(struct_ty, into.clone());
				}

				self.push_stmt(Stmt::Else);
				self.push_stmt(Stmt::Error("Invalid variant".into()));
				self.push_stmt(Stmt::End);
			}
		}
	}

	fn push_or(&mut self, into: Var, tys: &Vec<Ty<'_>>, discriminant_numty: NumTy, optional: bool) {
		let (into_ty_i_name, into_ty_i_expr) = self.add_occurrence("ty_i");

		self.push_local(into_ty_i_name, Some(self.readnumty(discriminant_numty)));
		let mut initial_if = true;
		let mut i_offset = 0usize;

		for ty in tys {
			let i = i_offset;

			match ty.primitive_ty() {
				PrimitiveTy::Enum(Enum::Unit(variants)) => {
					i_offset += variants.len();

					for (offset, variant) in variants.into_iter().enumerate() {
						let condition = into_ty_i_expr.clone().eq(((i + offset) as f64).into());
						if initial_if {
							self.push_stmt(Stmt::If(condition));
							initial_if = false;
						} else {
							self.push_stmt(Stmt::ElseIf(condition));
						}

						self.push_assign(into.clone(), Expr::Str(variant.to_string()));
					}
				}
				PrimitiveTy::Enum(Enum::Tagged { tag, variants }) => {
					i_offset += variants.len();

					for (offset, (variant, data)) in variants.into_iter().enumerate() {
						let condition = into_ty_i_expr.clone().eq(((i + offset) as f64).into());
						if initial_if {
							self.push_stmt(Stmt::If(condition));
							initial_if = false;
						} else {
							self.push_stmt(Stmt::ElseIf(condition));
						}

						self.push_assign(into.clone(), Expr::EmptyTable);
						self.push_assign(
							into.clone().eindex(Expr::Str(tag.to_string())),
							Expr::Str(variant.to_string()),
						);
						self.push_struct(&data, into.clone());
					}
				}
				PrimitiveTy::Unknown => {
					i_offset += 1;

					let condition = into_ty_i_expr.clone().eq((i as f64).into());
					if initial_if {
						self.push_stmt(Stmt::If(condition));
						initial_if = false;
					} else {
						self.push_stmt(Stmt::ElseIf(condition));
					}

					self.push_ty(&Ty::Unknown, into.clone());
				}
				PrimitiveTy::None(..) => unreachable!(),
				_ => {
					i_offset += 1;

					let condition = into_ty_i_expr.clone().eq((i as f64).into());
					if initial_if {
						self.push_stmt(Stmt::If(condition));
						initial_if = false;
					} else {
						self.push_stmt(Stmt::ElseIf(condition));
					}

					self.push_ty(ty, into.clone());
				}
			};
		}

		if optional {
			self.push_stmt(Stmt::ElseIf(into_ty_i_expr.clone().eq((i_offset as f64).into())));
			self.push_assign(into, Expr::Nil);
		}

		self.push_stmt(Stmt::Else);
		self.push_stmt(Stmt::Error("Invalid enumerator".into()));
		self.push_stmt(Stmt::End);
	}

	fn push_ty(&mut self, ty: &Ty, into: Var) {
		let into_expr = Expr::from(into.clone());

		match ty {
			Ty::Num(numty, range) => {
				self.push_assign(into, self.readnumty(*numty));

				if self.checks {
					self.push_range_check(into_expr, *range);
				}
			}

			Ty::Str(range) => {
				if let Some(len) = range.exact() {
					self.push_assign(into, self.readstring(len.into()));
				} else {
					let (len_name, len_expr) = self.add_occurrence("len");
					let (len_numty, len_offset) = range.numty();

					let mut offset_len_expr = self.readnumty(len_numty);
					if len_offset != 0.0 {
						offset_len_expr = offset_len_expr.add(Expr::Num(len_offset))
					}

					self.push_local(len_name.clone(), Some(offset_len_expr));

					if self.checks {
						self.push_range_check(len_expr.clone(), *range);
					}

					self.push_assign(into, self.readstring(len_expr.clone()));
				}
			}

			Ty::Buf(range) => {
				if let Some(len) = range.exact() {
					self.push_read_copy(into, len.into());
				} else {
					let (len_name, len_expr) = self.add_occurrence("len");
					let (len_numty, len_offset) = range.numty();

					let mut offset_len_expr = self.readnumty(len_numty);
					if len_offset != 0.0 {
						offset_len_expr = offset_len_expr.add(Expr::Num(len_offset))
					}

					self.push_local(len_name.clone(), Some(offset_len_expr));

					if self.checks {
						self.push_range_check(len_expr.clone(), *range);
					}

					self.push_read_copy(into, len_expr.clone())
				}
			}

			Ty::Arr(ty, range) => {
				self.push_assign(into.clone(), Expr::EmptyTable);

				let (var_name, var_expr) = self.add_occurrence("i");

				if let Some(len) = range.exact() {
					self.push_stmt(Stmt::NumFor {
						var: var_name.clone(),
						from: 1.0.into(),
						to: len.into(),
					});

					self.push_ty(ty, into.clone().eindex(var_expr.clone()));
					self.push_stmt(Stmt::End);
				} else {
					let (len_name, len_expr) = self.add_occurrence("len");
					let (len_numty, len_offset) = range.numty();

					let mut offset_len_expr = self.readnumty(len_numty);
					if len_offset != 0.0 {
						offset_len_expr = offset_len_expr.add(Expr::Num(len_offset))
					}

					self.push_local(len_name.clone(), Some(offset_len_expr));

					if self.checks {
						self.push_range_check(len_expr.clone(), *range);
					}

					self.push_stmt(Stmt::NumFor {
						var: var_name.clone(),
						from: 1.0.into(),
						to: len_expr.clone(),
					});

					let (inner_var_name, _) = self.add_occurrence("val");

					self.push_local(inner_var_name.clone(), None);

					self.push_ty(ty, Var::Name(inner_var_name.clone()));

					self.push_stmt(Stmt::Assign(
						into.clone().eindex(var_expr.clone()),
						Var::Name(inner_var_name.clone()).into(),
					));

					self.push_stmt(Stmt::End);
				}
			}

			Ty::Map(key, val) => {
				let length_numty = key.variants().map(|(numty, ..)| numty).unwrap_or(NumTy::U16);

				self.push_assign(into.clone(), Expr::EmptyTable);

				self.push_stmt(Stmt::NumFor {
					var: "_".into(),
					from: 1.0.into(),
					to: self.readnumty(length_numty),
				});

				let (key_name, key_expr) = self.add_occurrence("key");
				self.push_local(key_name.clone(), None);
				let (val_name, val_expr) = self.add_occurrence("val");
				self.push_local(val_name.clone(), None);

				self.push_ty(key, Var::Name(key_name.clone()));
				self.push_ty(val, Var::Name(val_name.clone()));

				self.push_assign(into.clone().eindex(key_expr.clone()), val_expr.clone());

				self.push_stmt(Stmt::End);
			}

			Ty::Set(key) => {
				let length_numty = key.variants().map(|(numty, ..)| numty).unwrap_or(NumTy::U16);

				self.push_assign(into.clone(), Expr::EmptyTable);

				self.push_stmt(Stmt::NumFor {
					var: "_".into(),
					from: 1.0.into(),
					to: self.readnumty(length_numty),
				});

				let (key_name, key_expr) = self.add_occurrence("key");
				self.push_local(key_name.clone(), None);

				self.push_ty(key, Var::Name(key_name.clone()));

				self.push_assign(into.clone().eindex(key_expr.clone()), Expr::True);

				self.push_stmt(Stmt::End);
			}

			Ty::Opt(ty) => {
				if let Ty::Or(tys, discriminant_numty) = &**ty {
					return self.push_or(into, tys, *discriminant_numty, true);
				}

				self.push_stmt(Stmt::If(self.readu8().eq(1.0.into())));

				if let Ty::Instance(class) = **ty {
					self.push_assign(Var::from("incoming_ipos"), Expr::from("incoming_ipos").add(1.0.into()));
					self.push_assign(
						into.clone(),
						Var::from("incoming_inst")
							.eindex(Var::from("incoming_ipos").into())
							.into(),
					);

					if self.checks && class.is_some() {
						self.push_assert(
							into_expr.clone().eq(Expr::Nil).or(Expr::Call(
								Box::new(into.clone()),
								Some("IsA".into()),
								vec![Expr::Str(class.unwrap().into())],
							)),
							format!("received instance is not of the {} class!", class.unwrap()),
						)
					}
				} else {
					self.push_ty(ty, into.clone())
				}

				self.push_stmt(Stmt::Else);
				self.push_assign(into, Expr::Nil);
				self.push_stmt(Stmt::End);
			}

			Ty::Ref(name, ..) => {
				self.push_assign(
					into,
					Expr::Call(
						Box::new(Var::from("types").nindex(format!("read_{name}"))),
						None,
						vec![],
					),
				);
			}

			Ty::Enum(enum_ty) => {
				self.push_assign(into.clone(), Expr::EmptyTable);
				self.push_enum(enum_ty, into)
			}

			Ty::Struct(struct_ty) => {
				self.push_assign(into.clone(), Expr::EmptyTable);
				self.push_struct(struct_ty, into)
			}

			Ty::Or(tys, discriminant_numty) => self.push_or(into, tys, *discriminant_numty, false),

			Ty::Instance(class) => {
				self.push_assign(Var::from("incoming_ipos"), Expr::from("incoming_ipos").add(1.0.into()));
				self.push_assign(
					into.clone(),
					Var::from("incoming_inst")
						.eindex(Var::from("incoming_ipos").into())
						.into(),
				);

				// always assert non-optional instances as roblox
				// will sometimes vaporize them
				self.push_assert(into_expr.clone().neq(Expr::Nil), "received instance is nil!".into());

				if self.checks && class.is_some() {
					self.push_assert(
						Expr::Call(
							Box::new(into),
							Some("IsA".into()),
							vec![Expr::Str(class.unwrap().into())],
						),
						format!("received instance is not of the {} class!", class.unwrap()),
					)
				}
			}

			Ty::Unknown => {
				self.push_assign(Var::from("incoming_ipos"), Expr::from("incoming_ipos").add(1.0.into()));
				self.push_assign(
					into.clone(),
					Var::from("incoming_inst")
						.eindex(Var::from("incoming_ipos").into())
						.into(),
				);
			}

			Ty::BrickColor => self.push_assign(
				into,
				Expr::Call(
					Box::new(Var::from("BrickColor").nindex("new")),
					None,
					vec![self.readu16()],
				),
			),

			Ty::DateTimeMillis => self.push_assign(
				into,
				Expr::Call(
					Box::new(Var::from("DateTime").nindex("fromUnixTimestampMillis")),
					None,
					vec![self.readf64()],
				),
			),

			Ty::DateTime => self.push_assign(
				into,
				Expr::Call(
					Box::new(Var::from("DateTime").nindex("fromUnixTimestamp")),
					None,
					vec![self.readf64()],
				),
			),

			Ty::Boolean => self.push_assign(into, self.readu8().eq(1.0.into())),

			Ty::Color3 => self.push_assign(
				into,
				Expr::Color3(
					Box::new(self.readu8()),
					Box::new(self.readu8()),
					Box::new(self.readu8()),
				),
			),

			Ty::Vector2 => self.push_assign(
				into,
				Expr::Call(
					Box::new(Var::from("Vector3").nindex("new")),
					None,
					vec![self.readf32(), self.readf32(), "0".into()],
				),
			),
			Ty::Vector3 => self.push_assign(into, self.readvector3()),
			Ty::Vector(x_ty, y_ty, z_ty) => {
				let x_numty = match **x_ty {
					Ty::Num(numty, range) => {
						if self.checks {
							self.push_range_check(into_expr.clone(), range);
						}

						numty
					}
					_ => unreachable!(),
				};
				let y_numty = match **y_ty {
					Ty::Num(numty, range) => {
						if self.checks {
							self.push_range_check(into_expr.clone(), range);
						}

						numty
					}
					_ => unreachable!(),
				};
				let z_numty = if let Some(z_ty) = z_ty {
					match **z_ty {
						Ty::Num(numty, range) => {
							if self.checks {
								self.push_range_check(into_expr.clone(), range);
							}

							Some(numty)
						}
						_ => unreachable!(),
					}
				} else {
					None
				};

				self.push_assign(into, self.readvector(x_numty, y_numty, z_numty));
			}

			Ty::AlignedCFrame => {
				let (axis_alignment_name, axis_alignment_expr) = self.add_occurrence("axis_alignment");

				self.push_local(axis_alignment_name, Some(self.readu8()));

				let (pos_name, pos_expr) = self.add_occurrence("pos");

				self.push_local(pos_name.clone(), Some(self.readvector3()));

				self.push_assign(
					into,
					Expr::Mul(
						Box::new(Expr::Call(
							Box::new(Var::from("CFrame").nindex("new")),
							None,
							vec![pos_expr.clone()],
						)),
						Box::new(
							Var::from("CFrameSpecialCases")
								.eindex(axis_alignment_expr.clone())
								.into(),
						),
					),
				);
			}
			Ty::CFrame => {
				let (pos_name, pos_expr) = self.add_occurrence("pos");
				self.push_local(pos_name.clone(), Some(self.readvector3()));
				let (axisangle_name, axisangle_expr) = self.add_occurrence("axisangle");
				self.push_local(axisangle_name.clone(), Some(self.readvector3()));
				let (angle_name, angle_expr) = self.add_occurrence("angle");
				self.push_local(
					angle_name,
					Some(Var::from(axisangle_name.as_str()).nindex("Magnitude").into()),
				);

				// We don't need to convert the axis back to a unit vector as the constructor does that for us
				// The angle is the magnitude of the axis vector
				// If the magnitude is 0, there is no rotation, so just make a cframe at the right position.
				// 	Trying to use fromAxisAngle in this situation gives NAN which is not ideal, so the branch is required.

				// if angle ~= 0 then
				//		value = CFrame.fromAxisAngle(axisangle, angle) + pos
				// else
				//		value = CFrame.new(pos)
				// end

				self.push_stmt(Stmt::If(Expr::Neq(Box::new(angle_expr.clone()), Box::new("0".into()))));
				self.push_assign(
					into.clone(),
					Expr::Add(
						Box::new(Expr::Call(
							Box::new(Var::from("CFrame").nindex("fromAxisAngle")),
							None,
							vec![axisangle_expr.clone(), angle_expr.clone()],
						)),
						Box::new(pos_expr.clone()),
					),
				);
				self.push_stmt(Stmt::Else);
				self.push_assign(
					into,
					Expr::Call(
						Box::new(Var::from("CFrame").nindex("new")),
						None,
						vec![pos_expr.clone()],
					),
				);
				self.push_stmt(Stmt::End);
			}
		}
	}
}

pub fn gen<'a, 'src: 'a, I>(
	types: I,
	names: &[String],
	checks: bool,
	var_occurrences: &mut HashMap<String, usize>,
) -> Vec<Stmt>
where
	I: IntoIterator<Item = &'a Ty<'src>>,
{
	Des {
		checks,
		buf: vec![],
		var_occurrences,
	}
	.gen(names, types.into_iter())
}
