-module(realm_patterns).
-export([smsg_auth_challenge/1, 
         cmsg_auth_session/1, 
         smsg_auth_response/0,
         smsg_char_enum/2,
         smsg_realm_split/0]).

-include("database_records.hrl").

-define(IN, /unsigned-little-integer).
-define(NI, /unsigned-big-integer).
-define(b,  /bytes).
-define(f,  /float-little).
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
    Opcode = realm_opcodes:c(smsg_char_enum),
    Chars  = realm_helper:chars(AccId, RealmId),
    CharB  = smsg_char_enum_build(Chars),
    Packet = <<(length(Chars))?B, CharB/binary>>,
    response(Opcode, Packet).

smsg_realm_split() ->
    Opcode = realm_opcodes:c(smsg_realm_split),
    Date   = list_to_binary("01/01/01"),
    Packet = <<16#FFFFFFFF?L, 0?L, Date/binary, 0?B>>,
    response(Opcode, Packet).

%% Internal use only

cmsg_auth_session_extract(<<0?B, Rest/bytes>>, Account) ->
    {Account, binary_to_list(Rest)};
cmsg_auth_session_extract(<<Letter?B, Rest/binary>>, Account) ->
    cmsg_auth_session_extract(Rest, Account ++ [Letter]).

smsg_char_enum_build(Chars) ->
    smsg_char_enum_build(Chars, <<>>).

smsg_char_enum_build([], Ready) ->
    Ready;
smsg_char_enum_build([Char|Chars], Ready) ->
    C = 
    <<(random:uniform(16#FFFFFFFF))?L, 0?L,        % guid 
      (list_to_binary(Char#char.name))/binary, 0?B,% char name
      (Char#char.player_bytes)/binary,             % 8 bytes: race, class, gender, skin, face,  
                                                   %          hair style, hair color, facial hair
      (Char#char.level)?B,                         % level
      (Char#char.zone_id)?L?IN,                    % zone id
      (Char#char.map_id)?L?IN,                     % map id
      (Char#char.position_x)?L?f,                  % x
      (Char#char.position_y)?L?f,                  % y
      (Char#char.position_z)?L?f,                  % z
      (Char#char.guild_id)?L?IN,                   % guild id
      (Char#char.general_flags)?L?IN,              % flags
      0?L,                                         % new in wolk
      1?B,                                         % rest state
      0?L,                                         % pet info
      0?L,                                         % pet level
      0?L,                                         % pet family
      (<<16#CA6A00000100000000:72>>)/binary,
      (<<16#7E2600000200000000:72>>)/binary,
      (<<16#D54700000300000000:72>>)/binary,
      (<<16#A32600000400000000:72>>)/binary,
      (<<16#997B00000500000000:72>>)/binary,
      (<<16#7EB500000600000000:72>>)/binary,
      (<<16#966E00000700000000:72>>)/binary,
      (<<16#5C6E00000800000000:72>>)/binary,
      (<<16#B76E00000900000000:72>>)/binary,
      (<<16#8D4800000A00000000:72>>)/binary,
      (<<16#6F2600000B00000000:72>>)/binary,
      (<<16#E00000000B00000000:72>>)/binary,
      (<<16#000000000000000000:72>>)/binary,
      (<<16#7B5200000C00000000:72>>)/binary,
      (<<16#C06B00001000000000:72>>)/binary,
      (<<16#122100000D00000000:72>>)/binary,
      (<<16#BA4A00000D00000000:72>>)/binary,
      (<<16#B65000001A00000000:72>>)/binary,
      (<<16#8D5000001300000000:72>>)/binary,
      (<<16#200A00001200000000:72>>)/binary
      % (smsg_char_enum_equip(Char#char.id))/binary  % equipment
      >>,
    smsg_char_enum_build(Chars, <<Ready/binary, C/binary>>).

smsg_char_enum_equip(CharId) ->
    smsg_char_enum_equip(char_helper:equipment(CharId), <<>>).

smsg_char_enum_equip([], Ready) -> Ready;
smsg_char_enum_equip([_|Items], Ready) ->
    smsg_char_enum_equip(Items, <<Ready/binary, 0?L?IN, 0?B, 0?L?IN>>).

response(Opcode, Packet) ->
    {<<(size(Packet)+2)?W?NI, Opcode?W?IN>>, <<Packet/binary, 0?B>>}.
