-module(packet_helper).

-export([read_cstring/1, make_cstring/1]).

%% @spec read_cstring(binary()) -> string().
read_cstring(Data) ->
    read_cstring(Data, "").

%% @spec read_cstring(binary(), string()) -> string().
read_cstring(<<0:8/integer, Rest/binary>>, String) ->
    {lists:reverse(String), Rest};
read_cstring(<<X:8/integer, Rest/binary>>, String) ->
    read_cstring(Rest, [X|String]).

%% @spec make_cstring(string()) -> binary().
make_cstring(String) ->
    <<(list_to_binary(String))/binary, 0:8/integer>>.