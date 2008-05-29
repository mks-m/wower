-module(logon_patterns).
-compile(export_all).

-define(QQ, :256/unsigned-little-integer).
-define(SH, :160/unsigned-little-integer).
-define(DQ, :128/unsigned-little-integer).
-define(Q,   :64/unsigned-little-integer).
-define(L,   :32/unsigned-little-integer).
-define(W,   :16/unsigned-little-integer).
-define(B,    :8/unsigned-little-integer).
-define(b,      /bytes).

auth_request(<<0?B, Err?B, Size?W, Game:4?b, Major?B, 
               Middle?B, Minor?B, Build?W, Platform:4?b, 
               Os:4?b, Country:4?b, TimeZone?L, IP:4?b, 
               Length?B, Account:Length?b>>) ->
    {ok, binary_to_list(Account)};
auth_request(_) ->
    no.
%%  {ok, auth_request, {Cmd, Err, Size, Game, Major, Middle, Minor, Build, 
%%                      Platform, Os, Country, TimeZone, IP, Account}};

auth_reply(H) ->
    <<0?B, 0?B, 0?B, (element(1, H))?QQ, 1?B, 7?B, 32?B, (element(2, H))?QQ, 
      (element(5, H))?QQ, (element(6, H))?DQ, 0?B>>.

auth_proof(<<1?B, A?QQ, M?SH, C?SH, N?B, _U?B>>) ->
    {ok, {A, M, C, N}};
auth_proof(_) ->
    no.
