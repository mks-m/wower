-module(logon_packets).
-compile(export_all).

-include("realm_records.hrl").

dispatch(Data, State) ->
    <<Opcode:8/integer, Rest/binary>> = Data,
    Handler = logon_opcodes:get(Opcode),
    ?MODULE:Handler(Opcode, Rest, State).

wrong_opcode(Opcode, _, State) ->
    io:format("unimplemented opcode ~p~n", [Opcode]),
    {skip, ok, State}.

wrong_packet(Handler, Data) ->
    io:format("wrong packet for ~p :~n~p", [Handler, Data]),
    ok.

wrong_code(Error) ->
    io:format("got error: ~p~n", [Error]).
