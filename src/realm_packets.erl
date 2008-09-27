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
    io:format("got header: ~p~n", [Header]),
    {DecryptedHeader, NewKey} = realm_crypto:decrypt(Header, State#client_state.key),
    io:format("dec header: ~p~n", [DecryptedHeader]),
    <<Size:16/integer-big, Opcode:16/integer-little, _:16>> = DecryptedHeader,
    io:format("packet size: ~p~n", [Size]),
    io:format("data size: ~p~n", [size(Data) + 4]),
    %% packet size check, should match
    Size    = size(Data) + 4,
    Handler = realm_opcodes:h(Opcode),
    io:format("handling: ~p~n", [Handler]),
    ?MODULE:Handler(Opcode, Data, State#client_state{key = NewKey}).

cmsg_auth_session(_Opcode, Rest, State) ->
    {_, A, _}      = realm_patterns:cmsg_auth_session(Rest),
    {Header, Data} = realm_patterns:smsg_auth_response(),
    io:format("~n~p~n~p~n", [Header, Data]),
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
