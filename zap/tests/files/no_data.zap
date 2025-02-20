event Test1 = {
    from: Client,
    type: Unreliable,
    call: SingleSync
}

event Test2 = {
    from: Client,
    type: Reliable,
    call: SingleSync
}

funct Test3 = {
    call: Sync,
}

funct Test4 = {
    call: Sync,
    args: u8,
}

funct Test5 = {
    call: Sync,
    rets: u8,
}
