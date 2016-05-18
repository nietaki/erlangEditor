## How to launch the code?
Under OSX or Linux install erlang and rebar. Open 4 console windows and run `make server` in the first one, and `make client`, `make client2`, `make client3` in the others.

You can do your typing in the clients and read the debug messages in the server.

You can also run `make test` to test the project

### Testing concurrent changes
It is difficult to see how the code works with concurrent changes when you run it on a single machine. But you can (sort of) using iTerm 2. Open a terminal window, split it (Shell -> split vertically), and launch a client in each. Then, right-click each of the window splits and from the context menu choose `Toggle Broadcasting Input`. You can now type to both clients at the same time!

Thanks to [@fala](https://github.com/fala) for pointing this out to me!

## How to run the benchmark?
In one console window run `make server` and leave it be.

In another console window run `make obs` to launch the observer/appmon. In the `Nodes` dropdown menu choose `server@localhost` and switch to the `Processes` tab. The thing you're looking for is the length of the `MsgQ` for `ledgerServer`. If it goes up beyond a very small integer (say 10), it means the configuration of benchmark proved too much for it - the server can't process the messages at the rate they're coming in.

In yet another console window run `make shell` to launch the erlang shell. In the erlang shell run `benchmark:run(ClientCount, ClientMovementsPerSecond)`, for example `benchmark:run(5, 10)` will mean launching 5 clients, each experiencing 10 location updates per second, caused by the user.

Please note: having the console debug messages on (`ledgerServer:console_debug_messsages_enabled()`) heavily impacts the performance.

### Problems with the benchmark
1. The clients and the server run on the same machine and cannibalize each other's resources. If the clients were remote, the results could have been different/more impressive.
1. %TODO
