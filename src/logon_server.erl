-module(logon_server).

-export([start/0, start/1, load/0, compile/0, stop/0, restart/1, loop/1]).

-define(OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

-include("logon_records.hrl").

start() ->
    crypto:start(),
    install(),
    mnesia:start(),
    mnesia:wait_for_tables([account, realm], 1000),
    tcp_server:start(?MODULE, 3724, {?MODULE, loop}).

stop() ->
    gen_server:call(?MODULE, stop).

restart(Method) ->
    stop(),
    ?MODULE:Method(),
    start().

loop(Socket) ->
    loop(Socket, #logon_state{}).

loop(Socket, State) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            case logon_packets:dispatch(Data, State) of
            {send, Response, NewState} ->
                gen_tcp:send(Socket, Response),
                loop(Socket, NewState);
            {skip, _, NewState} ->
                loop(Socket, NewState)
            end;
        {error, closed} ->
            ok
    end.

install() ->
    mnesia:delete_schema([node()]),
    mnesia:create_schema([node()]),
    mnesia:start(),
    
    mnesia:create_table(account, [{attributes, record_info(fields, account)},
                                  {disc_copies, [node()]}]),
    mnesia:dirty_write(account, #account{name     = "TEST", 
                                         password = "TEST"}),

    mnesia:wait_for_tables([account, realm], 1000),
    
    mnesia:create_table(realm, [{attributes, record_info(fields, realm)},
                                  {disc_copies, [node()]}]),
    mnesia:dirty_write(realm, #realm{name       = "Test Realm", 
                                     icon       = 0, 
                                     lock       = 0, 
                                     status     = 0, 
                                     address    = "127.0.0.1:8640", 
                                     population = 1.0, 
                                     characters = 3, 
                                     timezone   = 2}),
    mnesia:stop(),
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

start(Method) ->
    ?MODULE:Method(),
    start().

load() ->
    c:l(srp6),
    c:l(logon_server),
    c:l(logon_clients),
    c:l(logon_opcodes),
    c:l(logon_packets),
    c:l(logon_patterns).

compile() ->
    c:c(srp6),
    c:c(logon_server),
    c:c(logon_clients),
    c:c(logon_opcodes),
    c:c(logon_packets),
    c:c(logon_patterns).
