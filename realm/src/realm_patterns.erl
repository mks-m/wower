-module(realm_patterns).
-export([smsg_auth_challenge/1, 
         cmsg_auth_session/1, 
         smsg_auth_response/0,
         smsg_char_enum/2,
         cmsg_ping/1,
         cmsg_player_login/1,
		 cmsg_logout_request/2
		 ]).

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
    <<12?B, 0?L, 0?B, 0?L, 1?B>>.

smsg_char_enum(AccId, RealmId) ->
    Chars  = realm_helper:chars(AccId, RealmId),
    CharB  = smsg_char_enum_build(Chars),
    <<(length(Chars))?B, CharB/binary>>.

cmsg_ping(<<Sequence?L?IN, Latency?L?IN>>) ->
    {Sequence, Latency}.

cmsg_player_login(<<CharId?L?IN, 0?L>>) ->
    {ok, CharId};
cmsg_player_login(_) ->
    no.

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
    <<Ready/binary,
      (Char#char.id)?L?IN, 0?L,                    % guid 
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
      (smsg_char_enum_equip(Char#char.id))/binary  % equipment
      >>,
    smsg_char_enum_build(Chars, C).

smsg_char_enum_equip(CharId) ->
    smsg_char_enum_equip(char_helper:equipment(CharId), <<>>).

smsg_char_enum_equip([], Ready) -> Ready;
smsg_char_enum_equip([_|Items], Ready) ->
    smsg_char_enum_equip(Items, <<Ready/binary, 0?L?IN, 0?B, 0?L?IN>>).
	
cmsg_logout_request(S, C)->
	spawn(fun() ->
		receive
			{C, exit} -> ok
		after 20000 ->
			S ! {C, smsg_logout_complete, <<>>}
		end
		end
		).
