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

auth_request(<<0?B, _Err?B, _Size?W, _Game:4?b, _Major?B, 
               _Middle?B, _Minor?B, _Build?W, _Platform:4?b, 
               _Os:4?b, _Country:4?b, _TimeZone?L, _IP:4?b, 
               _Length?B, Account:_Length?b>>) ->
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
