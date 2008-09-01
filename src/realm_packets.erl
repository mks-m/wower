-module(realm_packets).
-compile(export_all).

-include("realm_records.hrl").
-define(B, /unsigned-integer).
-define(K, 20).

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
    {_, A, K}      = realm_patterns:cmsg_auth_session(Rest),
    {Header, Data} = realm_patterns:smsg_auth_response(),
    io:format("header: ~p~ndata: ~p~n", [Header, Data]),
    Crypt  = #crypt_state{si=0, sj=0, ri=0, rj=0, key=K},
    try encrypt(Header, Crypt) of
    {H, C} ->
        io:format("encrypted header: ~p~n", [H]),
        {send, <<H/binary, Data/binary>>, State#client_state{key=C, account=A}}
    catch
    T:E ->
        io:format("got error: ~n~p~n~p~n", [T, E]),
        io:format("stacktrace: ~n~p~n", [erlang:get_stacktrace()]),
        {skip, ok, State}
    end.

wrong_opcode(Opcode, _, State) ->
    io:format("unimplemented opcode ~p~n", [Opcode]),
    {skip, ok, State}.

wrong_packet(Handler, Data) ->
    io:format("wrong packet for ~p :~n~p", [Handler, Data]),
    ok.

wrong_code(Error) ->
    io:format("got error: ~p~n", [Error]).

encrypt(Header, Key) ->
    encrypt(Header, Key, <<>>).
encrypt(<<>>, Key, Result) -> {Result, Key};
encrypt(<<OldByte:8?B, Header/binary>>, #crypt_state{si = SI, sj = SJ, key = K}, Result) ->
    NewByte = lists:nth(SJ+1, K) bxor OldByte + SI,
    NewSJ   = (SJ + 1) rem ?K,
    encrypt(Header, #crypt_state{si  = NewByte, sj  = NewSJ, key = K}, <<Result/binary, NewByte:8>>).

decrypt(Header, Key) ->
    decrypt(Header, Key, <<>>).
decrypt(<<>>, Key, Result) -> {Result, Key};
decrypt(<<OldByte:8?B, Header/binary>>, #crypt_state{ri = RI, rj = RJ, key = K}, Result) ->
    NewByte = lists:nth(RJ+1, K) bxor (OldByte - RI),
    NewRJ   = (RJ + 1) rem ?K,
    encrypt(Header, #crypt_state{ri  = OldByte, rj  = NewRJ, key = K}, <<Result/binary, NewByte:8>>).
