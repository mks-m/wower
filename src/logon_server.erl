-module(logon_server).

-export([start/0, stop/0, restart/0, loop/1]).

-define(OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

-include("logon_records.hrl").

start() ->
    c:l(srp6),
    c:l(logon_server),
    c:l(logon_clients),
    c:l(logon_opcodes),
    c:l(logon_packets),
    c:l(logon_patterns),
    c:l(logon_records),
    crypto:start(),
    mnesia:start(),
    mnesia:wait_for_tables([account, realm], 20000),
    install(),
    tcp_server:start(?MODULE, 3724, {?MODULE, loop}).

stop() ->
    gen_server:call(?MODULE, stop).

restart() ->
    stop(),
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
    
    mnesia:create_table(account, [{attributes, record_info(fields, account)},
                                  {disc_copies, [node()]}]),
    mnesia:dirty_write(account, #account{name     = "TEST", 
                                         password = "TEST"}),
    
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
    ok.
