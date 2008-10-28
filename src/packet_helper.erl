-module(packet_helper).

-export([read_cstring/1, make_cstring/1]).

read_cstring(Data) ->
    read_cstring(Data, "").

read_cstring(<<0:8/integer, Rest/binary>>, String) ->
    {lists:reverse(String), Rest};
read_cstring(<<X:1/bytes, Rest/binary>>, String) ->
    read_cstring(Rest, [X|String]).

make_cstring(String) ->
    <<(list_to_binary(String))/binary, 0:8/integer>>.