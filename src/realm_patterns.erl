-module(realm_patterns).
-export([smsg_auth_challenge/1, cmsg_auth_session/1, smsg_auth_response/0]).

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

%% auth challenge
%% is sent just after client is connected
%%
%% long      opcode
%% long      random seed
%% byte      terminator
smsg_auth_challenge(Seed) ->
    Opcode = realm_opcodes:c(smsg_auth_challenge),
    <<8?W, Opcode?L?IN, Seed?L?IN, 0?B>>.

%% auth session
%%
%% long      build
%% long      unknown
%% cstr      acount name
%% byte N    session key
cmsg_auth_session(<<Build?L?IN, _Unk?L, Rest/binary>>) ->
    {Account, Key} = cmsg_auth_session_extract(Rest, ""),
    {Build, Account, Key};
cmsg_auth_session(_) ->
    no.

smsg_auth_response() ->
    Opcode = realm_opcodes:c(smsg_auth_response),
    Error  = realm_opcodes:e(auth_ok),
    Packet = <<Error, 0?L, 2?B, 0?L, 1?B>>,
    response(Opcode, Packet).

cmsg_auth_session_extract(<<0?B, Rest/binary>>, Account) ->
    {Account, Rest};
cmsg_auth_session_extract(<<Letter?B, Rest/binary>>, Account) ->
    cmsg_auth_session_extract(Rest, Account ++ [Letter]).

response(Opcode, Packet) ->
    {<<(size(Packet)+3)?W, Opcode?W?IN>>, <<Packet/binary, 0?B>>}.
