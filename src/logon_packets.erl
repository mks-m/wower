-module(logon_packets).
-compile(export_all).

match(_, []) ->
    fail;
match(Data, [Pattern|Rest]) ->
    case logon_patterns:Pattern(Data) of
        {ok, _, _} = Match -> Match;
        fail -> match(Data, Rest)
    end.

error(C) when atom(C) ->
    <<0:8, 0:8, (logon_opcodes:get(C)):8>>;
error(C) when integer(C) ->
    <<0:8, 0:8, C:8>>;
error(C) ->
    io:format("wrong errorcode: ~p~n", [C]),
    <<0:8, 0:8, 1:8>>.
