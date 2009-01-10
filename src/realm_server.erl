-module(realm_server).
-export([start/0, start/1, load/0, compile/0, stop/0, restart/1, loop/1]).
-export([sender/3, receiver/3]).

-include("realm_records.hrl").
-include("database_records.hrl").

start() ->
    crypto:start(),
    mnesia:start(),
    mnesia:wait_for_tables([account, realm, character], 1000),
    cell:start(),
    tcp_server:start(?MODULE, 8640, {?MODULE, loop}),
    io:format("realm server started ~n", []),
    ok.

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
    authenticate(Socket).

authenticate(Socket) ->
    Opcode = realm_opcodes:c(cmsg_auth_session),
    {ok, <<Size:16/integer-big, Opcode:32/integer-little>>} = gen_tcp:recv(Socket, 6),
    {ok, Data} = if Size-4 == 0 -> {ok, <<>>}; true -> gen_tcp:recv(Socket, Size-4) end,
    {O, D, A, EK, DK} = auth_session(Data),
    R = spawn_link(?MODULE, receiver, [Socket, self(), DK]),
    S = spawn_link(?MODULE, sender, [Socket, self(), EK]),
    S ! {self(), O, D},
    character:init(#client_state{realm=1, account=A, sender=S, receiver=R}).

sender(S, C, K) ->
    receive
    {C, Oa, D} ->
        Oi = realm_opcodes:c(Oa),
        H = <<(size(D)+2):16/integer-big, Oi:16/integer-little>>,
        {NH, NK} = realm_crypto:encrypt(H, K),
        gen_tcp:send(S, <<NH/binary, D/binary>>),
        sender(S, C, NK);
    Any ->
        io:format("unknown message: ~p~n", Any),
        sender(S, C, K)
    end.

receiver(S, C, K) ->
    {ok, H} = gen_tcp:recv(S, 6),
    {DH, NK} = realm_crypto:decrypt(H, K),
    <<Size:16/integer-big, Opcode:32/integer-little>> = DH,
    Handler = realm_opcodes:h(Opcode),
    io:format("handling: ~p (~p), size: ~p~n", [Handler, Opcode, Size]),
    dispatch(S, C, Size-4, Handler),
    receiver(S, C, NK).

dispatch(_, C, 0, H) ->
    C ! {self(), H, <<>>};
dispatch(S, C, Size, H) ->
    {ok, D} = gen_tcp:recv(S, Size),
    C ! {self(), H, D}.

start(Method) ->
    ?MODULE:Method(),
    start().

load() ->
    c:l(account_helper),
    c:l(char_helper),
    c:l(character),
    c:l(common_helper),
    c:l(packet_helper),
    c:l(realm_crypto),
    c:l(realm_helper),
    c:l(realm_opcodes),
    c:l(realm_patterns),
    c:l(srp6),
    c:l(tcp_server),
    c:l(update_fields).

compile() ->
    c:c(account_helper),
    c:c(char_helper),
    c:c(character),
    c:c(common_helper),
    c:c(packet_helper),
    c:c(realm_crypto),
    c:c(realm_helper),
    c:c(realm_opcodes),
    c:c(realm_patterns),
    c:c(srp6),
    c:c(tcp_server),
    c:c(update_fields).

auth_session(Rest) ->
    {_, A, _}      = realm_patterns:cmsg_auth_session(Rest),
    Data   = realm_patterns:smsg_auth_response(),
    K      = realm_crypto:encryption_key(A),
    EK     = #crypt_state{i=0, j=0, key=K},
    DK     = #crypt_state{i=0, j=0, key=K},
    [Account] = account_helper:find_by_name(A),
    {smsg_auth_response, Data, Account#account.id, EK, DK}.

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
