-module(logon_patterns).
-compile(export_all).

-include("logon_records.hrl").

-define(IN, /unsigned-little-integer).
-define(NI, /unsigned-big-integer).
-define(b,  /bytes).
-define(QQ, :256).
-define(SH, :160).
-define(DQ, :128).
-define(Q,   :64).
-define(L,   :32).
-define(W,   :16).
-define(B,    :8).

auth_request(<<0?B, _Err?B, _Size?W?IN, _Game:4?b, _Major?B, 
               _Middle?B, _Minor?B, _Build?W?IN, _Platform:4?b, 
               _Os:4?b, _Country:4?b, _TimeZone?L?IN, _IP:4?b, 
               _Length?B, Account:_Length?b>>) ->
    {ok, binary_to_list(Account)};
auth_request(_) ->
    no.
%%  {ok, auth_request, {Cmd, Err, Size, Game, Major, Middle, Minor, Build, 
%%                      Platform, Os, Country, TimeZone, IP, Account}};

auth_reply(H) ->
    <<0?B, 0?B, 0?B, (H#hash.public)?QQ?IN,
      1?B, 7?B, 32?B, (H#hash.modulus)?QQ?IN,
      (H#hash.salt)?QQ?IN, 0?DQ, 0?B >>.

auth_proof(<<1?B, A?QQ?IN, M?SH?IN, _C?SH?IN, _N?B, _U?B>>) ->
    {ok, {A, M}};
auth_proof(_) ->
    no.
