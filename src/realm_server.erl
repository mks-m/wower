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
    explain_exit(),
    Seed   = random:uniform(16#FFFFFFFF),
    Packet = realm_patterns:smsg_auth_challenge(Seed),
    gen_tcp:send(Socket, Packet),
    loop1(Socket, #client_state{realm=1}).

loop1(Socket, State) ->
    Opcode = realm_opcodes:c(cmsg_auth_session),
    {ok, <<Size:16/integer-big, Opcode:32/integer-little>>} = gen_tcp:recv(Socket, 6),
    io:format("handling: cmsg_auth_session (~p), size: ~p~n", [Opcode, Size]),
    dispatch(Socket, Size-4, cmsg_auth_session, State).

loop2(Socket, State) ->
    {ok, Header} = gen_tcp:recv(Socket, 6),
    {DecryptedHeader, NewKey} = realm_crypto:decrypt(Header, State#client_state.key),
    <<Size:16/integer-big, Opcode:32/integer-little>> = DecryptedHeader,
    Handler = realm_opcodes:h(Opcode),
    io:format("handling: ~p (~p), size: ~p~n", [Handler, Opcode, Size]),
    dispatch(Socket, Size-4, Handler, State#client_state{key = NewKey}).

dispatch(Socket, 0, Handler, State) ->
    proceed(Socket, realm_packets:Handler(<<>>, State));
dispatch(Socket, Size, Handler, State) ->
    {ok, Data} = gen_tcp:recv(Socket, Size),
    proceed(Socket, realm_packets:Handler(Data, State)).

proceed(Socket, {send, Response, State}) ->
    gen_tcp:send(Socket, Response),
    loop2(Socket, State);
proceed(Socket, {skip, _, State}) ->
    loop2(Socket, State).

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
    c:c(realm_helper).

explain_exit() ->
    Pid = self(),
    spawn(fun() -> 
		  process_flag(trap_exit, true),
		  link(Pid),
		  receive
		      {Type, Pid, Why} ->
			  io:format("process ~p: ~p~n", [Type, Why])
		  end
	  end).