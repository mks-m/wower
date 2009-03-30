-module(update_helper).
-export([block/2, packet/1, message/1]).

-include("common.hrl").
-include("database_records.hrl").

block(Type, Char) ->
    Target   = type(Type),
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
      4?B,                      % object type player

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

object(player) -> 4.

type(values)        -> 0;
type(movement)      -> 1;
type(create_object) -> 2;
type(create_self)   -> 3;
type(out_of_range)  -> 4;
type(in_range)      -> 5.

flag(none)         -> 16#0000;
flag(self)         -> 16#0001;
flag(transport)    -> 16#0002;
flag(has_target)   -> 16#0004;
flag(low_guid)     -> 16#0008;
flag(high_guid)    -> 16#0010;
flag(living)       -> 16#0020;
flag(has_position) -> 16#0040;
flag(vehicle)      -> 16#0080;
flag(unk1)         -> 16#0100;
flag(unk2)         -> 16#0200.

flags(List) ->
    flags(List, 0).

flags([], Flags) ->
    Flags;
flags([Flag|Rest], Flags) ->
    flags(Rest, Flags bor flag(Flag)).

guid(HG, LG) ->
    <<255?B, HG?L, LG?L>>.
