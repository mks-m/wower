-module(logon_server).

-export([start/0, stop/0, restart/0, listen/0, accept/2, install/0]).

-define(PORT, 3724).
-define(OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

-include("logon_records.hrl").

start() ->
    c:l(logon_server),
    c:l(logon_clients),
    c:l(logon_opcodes),
    c:l(logon_packets),
    c:l(logon_patterns),
    c:l(logon_records),
    crypto:start(),
    mnesia:start(),
    mnesia:wait_for_tables([account], 20000),
    install(),
    Pid = spawn(?MODULE, listen, []),
    register(?MODULE, Pid),
    ok.

stop() ->
    case whereis(?MODULE) of
        unknown -> not_running;
        Pid -> Pid ! stop
    end,
    ok.

restart() ->
    stop(),
    start().

install() ->
    mnesia:delete_schema([node()]),
    mnesia:create_schema([node()]),
    mnesia:create_table(account, [{attributes, record_info(fields, account)},
                                  {disc_copies, [node()]}]),
    mnesia:dirty_write(account, #account{name     = "TEST", 
                                         password = "TEST", 
                                         hash     = crypto:sha("TEST:TEST")}),
    ok.

listen() ->
    {ok, LSocket} = gen_tcp:listen(?PORT, ?OPTIONS),
    spawn(?MODULE, accept, [LSocket, []]),
    receive 
        stop ->
            io:format("stop received, closing socket~n", []), 
            gen_tcp:close(LSocket),
            ok
    end.

accept(LSocket, Clients) ->
    case gen_tcp:accept(LSocket) of
        {ok, Socket} ->
            Client = spawn(logon_clients, new, []),
            spawn(logon_packets, receiver, [Socket, Client]),
            accept(LSocket, [Socket | Clients]);
        {error, closed} ->
            io:format("socket closed, closing clients~n", []),
            [ gen_tcp:close(Socket) || Socket <- Clients ],
            ok
    end.
