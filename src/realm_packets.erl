-module(realm_packets).
-compile(export_all).

-include("realm_records.hrl").

dispatch(Data, #client_state{key=null, account=null} = State) ->
    <<_:16, Opcode:32/integer-little, Rest/binary>> = Data,
    io:format("got opcode: ~p~n", [Opcode]),
    Handler = realm_opcodes:h(Opcode),
    io:format("handling: ~p~n", [Handler]),
    ?MODULE:Handler(Opcode, Rest, State);
dispatch(<<Header:6/bytes, Data/binary>>, State) ->
    {_, Opcode} = decrypt(Header, State#client_state.key),
    io:format("got encrypted opcode: ~p~n", [Opcode]),
    Handler = realm_opcodes:h(Opcode),
    io:format("handling: ~p~n", [Handler]),
    ?MODULE:Handler(Opcode, Data, State).

cmsg_auth_session(_Opcode, Rest, State) ->
    {B, A, K} = realm_patterns:cmsg_auth_session(Rest),
    io:format("client build: ~p~naccount name: ~p~nsession key: ~p~n", [B, A, K]),
    {Header, Data} = realm_patterns:smsg_auth_response(),
    Crypt  = #crypt_state{si=0, sj=0, ri=0, rj=0, key=K},
    {H, K} = encrypt(Header, Crypt),
    {send, <<H/binary, Data/binary>>, State#client_state{key=Crypt, account=A}}.

wrong_opcode(Opcode, _, State) ->
    io:format("unimplemented opcode ~p~n", [Opcode]),
    {skip, ok, State}.

wrong_packet(Handler, Data) ->
    io:format("wrong packet for ~p :~n~p", [Handler, Data]),
    ok.

wrong_code(Error) ->
    io:format("got error: ~p~n", [Error]).

encrypt(Data, Key) ->
    ok.

decrypt(Header, Key) ->
    ok.