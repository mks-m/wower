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
    Packet = logon_patterns:smsg_auth_challenge(random:uniform(16#FFFFFFFF)),
    gen_tcp:send(Socket, Packet),
    loop(Socket, #realm_state{}).

loop(Socket, State) ->
    case gen_tcp:recv(Socket, 0) of
    {ok, Data} ->
        case realm_packets:dispatch(Data, State) of
        {send, Response, NewState} ->
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
    c:l(realm_server).

compile() ->
    c:c(srp6),
    c:c(realm_server).
