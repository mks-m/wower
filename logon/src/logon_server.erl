-module(logon_server).
-export([start/0, start/1, load/0, compile/0, stop/0, restart/1, loop/1]).

-export([receiver/2, sender/2]).

-include("logon_records.hrl").
-include("database_records.hrl").

start() ->
    crypto:start(),
    mnesia:start(),
    ets:new(connected_clients, [named_table, set, public]),
    mnesia:wait_for_tables([account, realm], 1000),
    tcp_server:start(?MODULE, 3724, {?MODULE, loop}),
    io:format("logon server started ~n", []),
    ok.

stop() ->
    gen_server:call(?MODULE, stop).

restart(Method) ->
    stop(),
    ?MODULE:Method(),
    start().

loop(Socket) ->
    R = spawn_link(?MODULE, receiver, [Socket, self()]),
    S = spawn_link(?MODULE, sender, [Socket, self()]),
    client(#logon_state{receiver = R, sender = S}).

client(#logon_state{receiver = R, sender = S} = State) ->
    receive
    {R, Opcode, Data} ->
        Handler = logon_opcodes:get(Opcode),
        case logon_packets:Handler(Opcode, Data, State) of
        {send, Response, NewState} ->
            S ! {self(), Response},
            client(NewState);
        {skip, _, NewState} ->
            client(NewState);
        Any ->
            io:format("unexpected response: ~p~n", Any),
            client(State)
        end;
    Any ->
        io:format("Weird message to client: ~p~n", [Any]),
        client(State)
    end.

receiver(Socket, Client) ->
    case gen_tcp:recv(Socket, 0) of
    {ok, Data} ->
        <<Opcode:8/integer, Rest/binary>> = Data,
        Client ! {self(), Opcode, Rest},
        receiver(Socket, Client);
    {error, closed} -> exit(socket_closed)
    end.

sender(Socket, Client) ->
    receive
        {Client, Data} ->
            gen_tcp:send(Socket, Data),
            sender(Socket, Client);
        Any ->
            io:format("Weird message to sender: ~p~n", [Any]),
            sender(Socket, Client)
    end.

start(Method) ->
    ?MODULE:Method(),
    start().

load() ->
    c:l(account_helper),
    c:l(char_helper),
    c:l(common_helper),
    c:l(logon_opcodes),
    c:l(logon_packets),
    c:l(logon_patterns),
    c:l(packet_helper),
    c:l(srp6),
    c:l(tcp_server).

compile() ->
    c:c(account_helper),
    c:c(char_helper),
    c:c(common_helper),
    c:c(logon_opcodes),
    c:c(logon_packets),
    c:c(logon_patterns),
    c:c(packet_helper),
    c:c(srp6),
    c:c(tcp_server).
