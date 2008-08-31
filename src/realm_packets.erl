-module(realm_packets).
-compile(export_all).

-include("realm_records.hrl").

dispatch(Data, State) ->
    io:format("got data: ~p~n", [Data]),
    <<Opcode:32/integer, Rest/binary>> = Data,
    io:format("got opcode: ~p~n", [Opcode]),
    Handler = realm_opcodes:get(Opcode),
    io:format("handling: ~p~n", [Handler]),
    ?MODULE:Handler(Opcode, Rest, State).

wrong_opcode(Opcode, _, State) ->
    io:format("unimplemented opcode ~p~n", [Opcode]),
    {skip, ok, State}.

wrong_packet(Handler, Data) ->
    io:format("wrong packet for ~p :~n~p", [Handler, Data]),
    ok.

wrong_code(Error) ->
    io:format("got error: ~p~n", [Error]).
