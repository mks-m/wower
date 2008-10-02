-module(realm_patterns).
-export([smsg_auth_challenge/1, 
         cmsg_auth_session/1, 
         smsg_auth_response/0,
         smsg_char_enum/2]).

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
    <<6?W?NI, Opcode?W?IN, Seed?L?IN>>.

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
    Packet = <<12?B, 0?L, 0?B, 0?L, 2?B>>,
    response(Opcode, Packet).

smsg_char_enum(AccId, RealmId) ->
    realm_helper:chars(AccId, RealmId).

%% Internal use only

cmsg_auth_session_extract(<<0?B, Rest/bytes>>, Account) ->
    {Account, binary_to_list(Rest)};
cmsg_auth_session_extract(<<Letter?B, Rest/binary>>, Account) ->
    cmsg_auth_session_extract(Rest, Account ++ [Letter]).

response(Opcode, Packet) ->
    {<<(size(Packet)+2)?W?NI, Opcode?W?IN>>, <<Packet/binary, 0?B>>}.
