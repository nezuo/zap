use crate::config::{Config, TyDecl};

use super::{ConfigProvider, Output};

struct TypesOutput<'src> {
	config: &'src Config<'src>,
	tabs: u32,
	buf: String,
}

impl Output for TypesOutput<'_> {
	fn push(&mut self, s: &str) {
		self.buf.push_str(s);
	}

	fn indent(&mut self) {
		self.tabs += 1;
	}

	fn dedent(&mut self) {
		self.tabs -= 1;
	}

	fn push_indent(&mut self) {
		for _ in 0..self.tabs {
			self.push("\t");
		}
	}
}

impl ConfigProvider for TypesOutput<'_> {
	fn get_config(&self) -> &Config {
		self.config
	}
}

impl<'a> TypesOutput<'a> {
	pub fn new(config: &'a Config) -> Self {
		Self {
			config,
			tabs: 0,
			buf: String::new(),
		}
	}

	fn push_tydecl(&mut self, tydecl: &TyDecl) {
		let name = &tydecl.name;
		let ty = &tydecl.ty;

		self.push_indent();
		self.push(&format!("export type {name} = "));
		self.push_ty(ty);
		self.push("\n");
	}

	fn push_tydecls(&mut self) {
		for tydecl in self.config.tydecls.iter() {
			self.push_tydecl(tydecl);
		}
	}

	pub fn output(mut self) -> String {
		self.push_line(&format!(
			"// Types generated by Zap v{} (https://github.com/red-blox/zap)",
			env!("CARGO_PKG_VERSION")
		));

		self.push_tydecls();

		self.buf
	}
}

pub fn code(config: &Config) -> Option<String> {
	Some(TypesOutput::new(config).output())
}
