-module(logon_patterns).
-compile(export_all).

-include("logon_records.hrl").

-define(IN, /unsigned-little-integer).
-define(NI, /unsigned-big-integer).
-define(b,  /bytes).
-define(f,  /float).
-define(QQ, :256).
-define(SH, :160).
-define(DQ, :128).
-define(Q,   :64).
-define(L,   :32).
-define(W,   :16).
-define(B,    :8).

%% auth logon request
%%
%% byte      cmd
%% byte      error
%% word      size
%% byte 4    gamename
%% byte      version major
%% byte      version middle
%% byte      version minor
%% long      version build
%% byte 4    platform
%% byte 4    os
%% byte 4    country
%% long      timezone_bias
%% long      ip
%% byte    L account name length
%% char L  N account name
auth_request(<<0?B, _Err?B, _Size?W?IN, _Game:4?b, _Major?B, 
               _Middle?B, _Minor?B, _Build?W?IN, _Platform:4?b, 
               _Os:4?b, _Country:4?b, _TimeZone?L?IN, _IP:4?b, 
               _Length?B, Account:_Length?b>>) ->
    {ok, binary_to_list(Account)}.

%% auth logon reply
%%
%% byte      cmd
%% byte      error
%% byte      unknown
%% i256      public ephemeral B
%% byte      generator length
%% byte      generator
%% byte      modulus length
%% i256      modulus
%% i256      salt
%% byte F    unknown
%% byte      terminator?
auth_reply(H) ->
    <<0?B, 0?B, 0?B, (H#hash.public)?QQ?IN,
      1?B, 7?B, 32?B, (H#hash.modulus)?QQ?IN,
      (H#hash.salt)?QQ?IN, 0?DQ, 0?B >>.

%% auth proof request
%%
%% byte      cmd
%% i256      public ephemeral A
%% sha1      client session proof M1
%%  crc      some CRC hash
%% byte      number of keys
%% byte      unknown
auth_proof(<<1?B, A?QQ?IN, M?SH?IN, _C?SH?IN, _N?B, _U?B>>) ->
    {ok, {A, M}};
auth_proof(_) ->
    no.

%% auth proof reply
%%
%% byte      cmd
%% i256      server session proof M2
%% byte 9    unknown
%% byte      terminator?
auth_reproof(H) ->
    <<1?B, 0?B, (H#hash.session_proof)?SH?IN, 0?Q, 0?W>>.

%% realmlist request
%%
%% byte      cmd
%% byte 5    unknown
realmlist_request(<<16?B, _Rest?b>>) ->
    {ok}.

%% realmlist reply
%%
%% byte      cmd               16
%% word      packet size
%% long      start             0
%% word      realms count
%% realms:
%%   byte      icon            0...4
%%   byte      lock            1 | 0
%%   byte      online status   1 | 0
%%   long      realm id ?
%%   char N    realm name
%%   char N    ip address
%%   real      population
%%   byte      characters
%%   byte      timezone
%%   byte      unknown         0x2C
%% byte      unknown           0x10
%% byte      terminator?       0
realmlist_reply(Realms) ->
    BinaryRealms = realmlist_build(Realms),
    <<16?B, (size(BinaryRealms) + 7)?W?IN, 0?L, (length(Realms))?W, BinaryRealms/binary, 0?B>>,
    <<16?B, 8?W?IN, 0?L, 0?W, 16#15?B, 0?B>>.

%% realmlist helper function
realmlist_build([]) ->
    <<16?B>>;
realmlist_build([R|Realms]) ->
    <<0?B, 0?B, 0?B, (list_to_binary(R#realm.name))/binary, 0?B, 
      (list_to_binary(R#realm.address))/binary, 0?B, (0.0)?f, 
      0?B, 0?B, 0?B, (realmlist_build(Realms))?b>>.
