<script setup lang="ts">
const eventConfigFile = `event MyEvent = {
    from: Server,
    type: Reliable,
    call: ManyAsync,
    data: struct {
        foo: string,
        bar: u8,
    }
}

event AnotherEvent = {
    from: Client,
    type: Reliable,
    call: SingleAsync,
    data: (Foo: boolean, Bar: u8)
}
`

const functConfigFile = `funct GetScore = {
    call: Async,
    args: (roundId: string, category: enum { HighScore, LowScore, AverageScore }),
    rets: u16,
}
`
</script>

# Using Your Generated Code

After [generating code](./generation.md), Zap provides two output files: the client API and the server API. Detailed documentation for both is provided below. 

::: info
This page will assume you're using the default casing value of `PascalCase`.

Learn more about casing options [here](../config/options.md#casing).
:::

## Event API

In this section, we use the following Zap file as an example.

<CodeBlock :code="eventConfigFile" />

### Listening to Events

Listening to events works the same on both the server and client. It is assumed that `Zap` is a properly defined reference to the generated API.

If your event's [call field](../config/events.md#call) is `SingleAsync` or `SingleSync` you can assign only one listener the `SetCallback` function.

```lua
-- only server listeners are given the player argument
Zap.AnotherEvent.SetCallback(function(Player, Foo, Bar)
    -- Do something with the player and data
end)
```

If your event's [call field](../config/events.md#call) is `ManyAsync` or `ManySync` you can assign multiple listeners using the `On` function.

```lua
local Disconnect = Zap.MyEvent.On(function(Options)
    -- Do something with the data
end)

-- Disconnect the listener after 10 seconds
task.delay(10, Disconnect)
```

As shown above, the `On` function for `Many` style events returns a `Disconnect` function, which can be used to remove the listener.

::: danger
Remember that synchronous event callbacks must not yield or error:

- If a sync callback yields, it will cause undefined and game-breaking behavior.
- If a sync callback errors, it will drop the packet.

Use `Sync` events only when performance is critical.
:::

## Client Event API

The client has a single function for firing events, `Fire`, which takes the event's data as its arguments.

```lua
Zap.AnotherEvent.Fire(true, 32)
```

## Server Event API

The server has many functions for firing events, each with their own use case.

::: tip
`FireAll`, `FireExcept`, `FireList`, and `FireSet` serialize the event's data only once, making them more efficient than firing the event individually to each player.

Use these functions when sending the same data to multiple players.
:::

### Fire

The `Fire` function takes a player and the event's data as its arguments.

```lua
Zap.MyEvent.Fire(Player, {
    foo = "baz",
    bar = 1,
})
```

### FireAll

The `FireAll` function takes the event's data as its arguments. It will fire the event to all players.

::: info
`FireAll` can be disabled using the [disable_fire_all](../config/options.md#disable-fire-all) option. Attempting to call `FireAll` when `disable_fire_all = true` will produce an error.
:::

```lua
Zap.MyEvent.FireAll({
    foo = "baz",
    bar = 1,
})
```

### FireExcept

The `FireExcept` function takes a player and the event's data as its arguments. It will fire the event to all players except the specified player.

```lua
Zap.MyEvent.FireExcept(Player, {
    foo = "baz",
    bar = 1,
})
```

### FireList

The `FireList` function takes a list of players and the event's data as its arguments. It will fire the event to all players in the list.

```lua
Zap.MyEvent.FireList({ Player1, Player2 }, {
    foo = "baz",
    bar = 1,
})
```

### FireSet

The `FireSet` function takes a set of players and the event's data as its arguments. It will fire the event to all players in the set.

::: tip
A [set](../config/types.md#sets) is a table where the keys are of a certain datatype and the values are always `True`.
:::

```lua
Zap.MyEvent.FireSet({
    [Player1] = true,
    [Player2] = true,
}, {
    foo = "baz",
    bar = 1,
})
```

## Function API

In this section, we use the following Zap file as an example.

<CodeBlock :code="functConfigFile" />

## Client Function API

### Call

The client has a single function for invoking the server, `Call`, which takes the function's data as its arguments.

```lua
local score = Zap.GetScore.Call("round_ipsum-lorem", "LowScore")
print(score)
```

## Server Function API

### SetCallback
The server has a single function for responding to client invocations, `SetCallback`. This is similar to `Zap.MyEvent.SetCallback`, but it instead has a return as defined by the Zap config.

```lua
local function handleRequest(Player: Player, roundId: string, category: "HighScore" | "LowScore" | "AverageScore"): number
    -- Do something with the data.
    return 0 -- We must return a u16 according to our Zap config.
end

Zap.GetScore.SetCallback(handleRequest)
```
