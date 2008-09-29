-module(realm_server).
-export([start/0, start/1, load/0, compile/0, stop/0, restart/1, loop/1]).

-include("realm_records.hrl").

start() ->
    crypto:start(),
    mnesia:start(),
    mnesia:wait_for_tables([account, realm], 1000),
    tcp_server:start(?MODULE, 8640, {?MODULE, loop}).

stop() ->
    gen_server:call(?MODULE, stop).

restart(Method) ->
    stop(),
    ?MODULE:Method(),
    start().

loop(Socket) ->
    Seed   = random:uniform(16#FFFFFFFF),
    Packet = realm_patterns:smsg_auth_challenge(Seed),
    gen_tcp:send(Socket, Packet),
    loop(Socket, #client_state{}).

loop(Socket, State) ->
    case gen_tcp:recv(Socket, 0) of
    {ok, Data} ->
        case realm_packets:dispatch(Data, State) of
        {send, Response, NewState} ->
            io:format("sending: ~p~n", [Response]),
            gen_tcp:send(Socket, Response),
            loop(Socket, NewState);
        {skip, _, NewState} ->
            loop(Socket, NewState);
        Anything ->
            io:format("unexpected response: ~p~n", Anything),
            loop(Socket, State)
        end;
    {error, closed} ->
        ok
    end.

start(Method) ->
    ?MODULE:Method(),
    start().

load() ->
    c:l(srp6),
    c:l(realm_opcodes),
    c:l(realm_patterns),
    c:l(realm_packets),
    c:l(realm_helper).

compile() ->
    c:c(srp6),
    c:c(realm_opcodes),
    c:c(realm_patterns),
    c:c(realm_packets),
    c:l(realm_helper).
