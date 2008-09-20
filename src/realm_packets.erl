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
    io:format("encrypted opcode: ~p~n", [Header]),
    {_, Opcode} = realm_crypto:decrypt(Header, State#client_state.key),
    io:format("decrypted opcode: ~p~n", [Opcode]),
    Handler = realm_opcodes:h(Opcode),
    io:format("handling: ~p~n", [Handler]),
    ?MODULE:Handler(Opcode, Data, State).

cmsg_auth_session(_Opcode, Rest, State) ->
    {_, A, _}      = realm_patterns:cmsg_auth_session(Rest),
    {Header, Data} = realm_patterns:smsg_auth_response(),
    K      = realm_crypto:encryption_key(A),
    Crypt  = #crypt_state{si=0, sj=0, ri=0, rj=0, key=K},
    {H, C} = realm_crypto:encrypt(Header, Crypt),
    {send, <<H/binary, Data/binary>>, State#client_state{key=C, account=A}}.

wrong_opcode(Opcode, _, State) ->
    io:format("unimplemented opcode ~p~n", [Opcode]),
    {skip, ok, State}.

wrong_packet(Handler, Data) ->
    io:format("wrong packet for ~p :~n~p", [Handler, Data]),
    ok.

wrong_code(Error) ->
    io:format("got error: ~p~n", [Error]).

