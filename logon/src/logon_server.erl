-module(logon_server).
-export([start/0, stop/0, restart/0, loop/1]).

-export([receiver/2, sender/2]).

-include("logon_records.hrl").
-include("database_records.hrl").

start() ->
    load(),
    crypto:start(),
    mnesia:start(),
    ets:new(connected_clients, [named_table, set, public]),
    mnesia:wait_for_tables([account, realm], 1000),
    tcp_server:start(?MODULE, 3724, {?MODULE, loop}),
    io:format("logon server started ~n", []),
    ok.

stop() ->
    gen_server:call(?MODULE, stop).

restart() ->
    stop(),
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
    {ok, Data} = gen_tcp:recv(Socket, 0),
    <<Opcode:8/integer, Rest/binary>> = Data,
    Client ! {self(), Opcode, Rest},
    receiver(Socket, Client).

sender(Socket, Client) ->
    receive
        {Client, Data} ->
            gen_tcp:send(Socket, Data),
            sender(Socket, Client);
        Any ->
            io:format("Weird message to sender: ~p~n", [Any]),
            sender(Socket, Client)
    end.

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
