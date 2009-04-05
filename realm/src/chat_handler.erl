-module(chat_handler).

-export([cmsg_get_channel_member_count/3,
         cmsg_join_channel/3]).
-import(packet_helper, [read_cstring/1,
                        make_cstring/1]).

-include("common.hrl").

%% @spec cmsg_get_channel_member_count(pid(), tuple(), binary()) -> tuple().
cmsg_get_channel_member_count(S, St, Data) ->
    {Name, _} = read_cstring(Data),
    S ! {self(), smsg_channel_member_count, <<(make_cstring(Name))/binary, 0?B, 0?L>>},
    St.

%% @spec cmsg_join_channel(pid(), tuple(), binary()) -> tuple().
cmsg_join_channel(_, St, _) ->
    St.