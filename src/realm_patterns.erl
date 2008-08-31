-module(realm_patterns).
-compile(export_all).

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
    Packet = <<Opcode?L?IN, Seed?L?IN, 0?B>>,
    <<(size(Packet))?W, Packet/binary>>.