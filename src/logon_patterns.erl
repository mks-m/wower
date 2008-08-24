-module(logon_patterns).
-compile(export_all).

-include("logon_records.hrl").

-define(IN, /unsigned-little-integer).
-define(QQ, :256?IN).
-define(SH, :160?IN).
-define(DQ, :128?IN).
-define(Q,   :64?IN).
-define(L,   :32?IN).
-define(W,   :16?IN).
-define(B,    :8?IN).
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
    <<0?B, 0?B, 0?B, (H#hash.public)?QQ,
      1?B, 7?B, 32?B, (H#hash.modulus)?QQ,
      (H#hash.salt)?QQ, 0?DQ, 0?B >>.

auth_proof(<<1?B, A?QQ, M?SH, _C?SH, _N?B, _U?B>>) ->
    {ok, {A, M}};
auth_proof(_) ->
    no.
