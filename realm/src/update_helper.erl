-module(update_helper).
-export([block/2, packet/1, message/1]).

-include("common.hrl").
-include("database_records.hrl").

%% A9 packet description
%% smsg_update_object / smsg_compressed_update_object
%%
%% long       N         number of update blocks
%% - N times
%%     byte             update type
%%     guid             object guid
%%     byte             object type
%%     byte             update flags
%%     long             movement flags
%%     word             unknown
%%     long             game time
%%     float     4      x, y, z and orientation
%%     long             falling time
%%     float     9      speeds: walk, run, walk back, 
%%                              swim, swim back, fly
%%                              fly back, turn, pitch
%%     byte   M         number of long's after this byte
%%     long      M      bitmask in which i'th bit means if i'th 
%%                      field from object's blob is present

-record(update_block, {update_type,
                       object_guid,
                       object_type,
                       update_flags,
                       movement_flags,
                       unknown = 0,
                       game_time,
                       position,
                       fall_time,
                       speeds,
                       mask,
                       fields}).

block(Type, Char) ->
    Target   = update_type(Type),
    GameTime = common_helper:ms_time(),
    UB       = char_helper:unit_bytes_0(Char),
    PB1      = char_helper:player_bytes(Char),
    PB2      = char_helper:player_bytes_2(Char),
    BitMask  = update_fields:mask([{object, guid},
                                   {object, guid_2},
                                   {object, type},
                                   {object, scale_x},
                                   {unit, bytes_0},
                                   {unit, health},
                                   {unit, maxhealth},
                                   {unit, level},
                                   {unit, factiontemplate},
                                   {unit, displayid},
                                   {unit, dynamic_flags},
                                   {player, player_bytes},
                                   {player, player_bytes_2}]),
    Flags  = flags([self, living, position]),
    <<Target?B,                 % update target

      (guid(Char#char.id, 0))/binary,
      (typeid(player))?B,       % object type player

      Flags?B,                  % update flags
      0?L,                      % move flags
      0?W,                      % unknown

      GameTime?L,               % current time

      (Char#char.position_x)?f, % position x
      (Char#char.position_y)?f, % position y
      (Char#char.position_z)?f, % position z
      (Char#char.orientation)?f,% orientation

      0?L,                      % fall time

      2.5?f,                    % walk speed
      7?f,                      % run speed
      4.5?f,                    % walk back speed
      4.722222?f,               % swim speed
      2.5?f,                    % swim back speed
      7?f,                      % fly speed
      4.5?f,                    % fly back speed
      3.141593?f,               % turn speed
      1.0?f,                    % pitch speed

      (size(BitMask) div 4)?B,  % number of long's
      BitMask/binary,           % bitmask

      (Char#char.id)?L, 0?L,    % player guid
      25?L,                     % player type
      (Char#char.scale)?f,
      UB?L,                     % race, class, gender, power
      (Char#char.health)?L,
      (Char#char.health)?L,     % max health
      (Char#char.level)?L,
      (Char#char.faction_template)?L,
      (Char#char.display_id)?L,
      0?L,                      % dynamic flag (0 = alive)
      PB1?L,                    % skin, face, hair style, hair color
      PB2?L                     % facial hair, unknown
      >>.

packet(Blocks) ->
    L = length(Blocks),
    packets(Blocks, <<L?L>>).

packets([], Result) ->
    Result;
packets([Block|Rest], Result) ->
    packets(Rest, <<Result/binary, Block/binary>>).

message(Packet) ->
    S = size(Packet),
    if S > 50 ->
        Compressed = compress(Packet),
        {self(), smsg_compressed_update_object, Compressed};
    true ->
        {self(), smsg_update_object, Packet}
    end.

compress(Packet) ->
    Z  = zlib:open(),
    ok = zlib:deflateInit(Z, best_speed),
    P  = zlib:deflate(Z, Packet),
    L  = zlib:deflate(Z, [], finish),
    ok = zlib:deflateEnd(Z),
    zlib:close(Z),
    list_to_binary([P|L]).

update_type(values)         -> 0;
update_type(movement)       -> 1;
update_type(create_object)  -> 2;
update_type(create_object2) -> 3;
update_type(out_of_range)   -> 4;
update_type(in_range)       -> 5.

update_flag(none)         -> 16#0000;
update_flag(self)         -> 16#0001;
update_flag(transport)    -> 16#0002;
update_flag(has_target)   -> 16#0004;
update_flag(low_guid)     -> 16#0008;
update_flag(high_guid)    -> 16#0010;
update_flag(living)       -> 16#0020;
update_flag(has_position) -> 16#0040;
update_flag(vehicle)      -> 16#0080;
update_flag(unk1)         -> 16#0100;
update_flag(unk2)         -> 16#0200.

typeid(object)         -> 0;
typeid(item)           -> 1;
typeid(container)      -> 2;
typeid(unit)           -> 3;
typeid(player)         -> 4;
typeid(game_object)    -> 5;
typeid(dynami_cobject) -> 6;
typeid(corpse)         -> 7;
typeid(ai_group)       -> 8;
typeid(area_trigger)   -> 9;
typeid(pet)            -> typeid(unit).

update_flags(List) ->
    update_flags(List, 0).

update_flags([], Flags) ->
    Flags;
update_flags([Flag|Rest], Flags) ->
    update_flags(Rest, Flags bor update_flag(Flag)).

guid(HG, LG) ->
    <<255?B, HG?L, LG?L>>.
