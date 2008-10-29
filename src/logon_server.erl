-module(logon_server).
-export([start/0, start/1, load/0, compile/0, stop/0, restart/1, loop/1]).

-include("logon_records.hrl").
-include("database_records.hrl").

start() ->
    crypto:start(),
    mnesia:start(),
    ets:new(connected_clients, [named_table, set, public]),
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
    try gen_tcp:recv(Socket, 0) of
    {ok, Data} ->
        case logon_packets:dispatch(Data, State) of
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
    catch
    _ -> 
        ok
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
