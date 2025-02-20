opt call_default = "Polling"
event polling = {
    from: Server,
    type: Reliable,
    data: (u8?, u8?),
}
