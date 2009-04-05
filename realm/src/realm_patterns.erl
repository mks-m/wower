-module(realm_patterns).
-export([smsg_auth_challenge/1, 
         cmsg_auth_session/1, 
         smsg_auth_response/0,
         smsg_char_enum/2,
         cmsg_ping/1,
         cmsg_player_login/1
		 ]).

-include("database_records.hrl").
-include("common.hrl").

%% auth challenge
%% is sent just after client is connected
%%
%% long      opcode
%% long      random seed
%% byte      terminator
%%
%% @spec smsg_auth_challenge(int()) -> binary().
smsg_auth_challenge(Seed) ->
    Opcode = realm_opcodes:c(smsg_auth_challenge),
    <<6:16?O, Opcode?W, Seed?L>>.

%% auth session
%%
%% long      build
%% long      unknown
%% cstr      acount name
%% byte N    session key
%%
%% @spec cmsg_auth_session(binary()) -> {int(), string(), binary()} |
%%                                      {error, bad_cmsg_auth_session}.
cmsg_auth_session(<<Build?L, _Unk?L, Rest/binary>>) ->
    {Account, Key} = cmsg_auth_session_extract(Rest, ""),
    {Build, Account, Key};
cmsg_auth_session(_) ->
    {error, bad_cmsg_auth_session}.

%% @spec smsg_auth_response() -> binary().
smsg_auth_response() ->
    <<12?B, 0?L, 0?B, 0?L, 1?B>>.

%% @spec smsg_char_enum(int(), int()) -> binary().
smsg_char_enum(AccId, RealmId) ->
    Chars  = realm_helper:chars(AccId, RealmId),
    CharB  = smsg_char_enum_build(Chars),
    <<(length(Chars))?B, CharB/binary>>.

%% @spec cmsg_ping(binary()) -> {int(), int()}.
cmsg_ping(<<Sequence?L, Latency?L>>) ->
    {Sequence, Latency}.

%% @spec cmsg_player_login(binary()) -> {ok, int()} |
%%                                      {error, bad_cmsg_player_login}.
cmsg_player_login(<<CharId?L, 0?L>>) ->
    {ok, CharId};
cmsg_player_login(_) ->
    {error, bad_cmsg_player_login}.

%% Internal use only

%% @spec cmsg_auth_session_extract(binary()) -> {string(), list()}.
cmsg_auth_session_extract(<<0?B, Rest/bytes>>, Account) ->
    {Account, binary_to_list(Rest)};
cmsg_auth_session_extract(<<Letter?B, Rest/binary>>, Account) ->
    cmsg_auth_session_extract(Rest, Account ++ [Letter]).

%% @spec smsg_char_enum_build([tuple()]) -> binary().
smsg_char_enum_build(Chars) ->
    smsg_char_enum_build(Chars, <<>>).

%% @spec smsg_char_enum_build([tuple()], binary()) -> binary().
smsg_char_enum_build([], Ready) ->
    Ready;
smsg_char_enum_build([Char|Chars], Ready) ->
    C = 
    <<Ready/binary,
      (Char#char.id)?L, 0?L,                       % guid
      (list_to_binary(Char#char.name))/binary, 0?B,% char name
      (char_helper:race(Char#char.race))?B,        % race
      (char_helper:class(Char#char.class))?B,      % class
      (char_helper:gender(Char#char.gender))?B,    % gender
      (Char#char.skin)?B,                          % skin
      (Char#char.face)?B,                          % face
      (Char#char.hair_style)?B,                    % hair style
      (Char#char.hair_color)?B,                    % hair color
      (Char#char.facial_hair)?B,                   % facial hair
      (Char#char.level)?B,                         % level
      (Char#char.zone_id)?L,                       % zone id
      (Char#char.map_id)?L,                        % map id
      (Char#char.position_x)?f,                    % x
      (Char#char.position_y)?f,                    % y
      (Char#char.position_z)?f,                    % z
      (Char#char.guild_id)?L,                      % guild id
      (Char#char.general_flags)?L,                 % flags
      0?L,                                         % new in wolk
      1?B,                                         % rest state
      0?L,                                         % pet info
      0?L,                                         % pet level
      0?L,                                         % pet family
      (smsg_char_enum_equip(Char#char.id))/binary  % equipment
      >>,
    smsg_char_enum_build(Chars, C).

%% @spec smsg_char_enum_equip(int()) -> binary().
smsg_char_enum_equip(CharId) ->
    smsg_char_enum_equip(char_helper:equipment(CharId), <<>>).

%% @spec smsg_char_enum_equip(list()) -> binary().
smsg_char_enum_equip([], Ready) -> Ready;
smsg_char_enum_equip([_|Items], Ready) ->
    smsg_char_enum_equip(Items, <<Ready/binary, 0?L, 0?B, 0?L>>).
