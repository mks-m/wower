-module(realm_packets).
-compile(export_all).

-include("realm_records.hrl").

dispatch(Data, State) ->
    <<_:16, Opcode:32/integer-little, Rest/binary>> = Data,
    io:format("got opcode: ~p~n", [Opcode]),
    Handler = realm_opcodes:h(Opcode),
    io:format("handling: ~p~n", [Handler]),
    ?MODULE:Handler(Opcode, Rest, State).

cmsg_auth_session(_Opcode, Rest, _State) ->
    {B, A, K} = realm_patterns:cmsg_auth_session(Rest),
    io:format("client build: ~p~naccount name: ~p~nsession key: ~p~n", [B, A, K]).

wrong_opcode(Opcode, _, State) ->
    io:format("unimplemented opcode ~p~n", [Opcode]),
    {skip, ok, State}.

wrong_packet(Handler, Data) ->
    io:format("wrong packet for ~p :~n~p", [Handler, Data]),
    ok.

wrong_code(Error) ->
    io:format("got error: ~p~n", [Error]).
