-module(chat_handler).

-export([cmsg_get_channel_member_count/3,
         cmsg_join_channel/3]).
-import(packet_helper, [read_cstring/1,
                        make_cstring/1]).

-define(I, /unsigned-little-integer).
-define(O, /unsigned-big-integer).
-define(b,  /bytes).                   % that's for plain text
-define(f,  :32/float-little).         % float values obviously
-define(SH, :160?I).                   % SH is for sha1
-define(Q,   :64?I).                   % uint64 - Q for quad
-define(L,   :32?I).                   % uint32 - L for long
-define(W,   :16?I).                   % uint16 - W for word
-define(B,    :8).                     % byte (doesn't need to be endianated)

cmsg_get_channel_member_count(S, St, Data) ->
    {Name, _} = read_cstring(Data),
    S ! {self(), smsg_channel_member_count, <<(make_cstring(Name))/binary, 0?B, 0?L>>},
    St.

cmsg_join_channel(_, St, _) ->
    St.