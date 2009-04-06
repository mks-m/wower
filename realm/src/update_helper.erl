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

%% @spec block(atom(), tuple()) -> tuple().
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
    #update_block{
        update_type    = Target,
        object_guid    = Char#char.id,
        object_type    = player,
        update_flags   = [self, living, has_position],
        movement_flags = 0,
        game_time      = GameTime,
        position       = {Char#char.position_x,
                          Char#char.position_y,
                          Char#char.position_z,
                          Char#char.orientation},
        fall_time      = 0,
        speeds         = {2.5, 7, 4.5, 4.72, 2.5,
                          7, 4.5, 3.141593, 1.0},
        mask           = BitMask,
        fields         = <<
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
        >>}.

%% @spec packet([tuple()]) -> binary().
packet(Blocks) ->
    L = length(Blocks),
    packets(Blocks, <<L?L>>).

%% @spec packets(list(), binary()) -> binary().
packets([], Result) ->
    Result;
packets([B|Rest], Result) ->
    {X, Y, Z, O} = B#update_block.position,
    {W, R, WB, S, SB, F, FB, T, P} = B#update_block.speeds,
    Binary = <<(B#update_block.update_type)?B,
               (guid(B#update_block.object_guid, 0))/binary,
               (typeid(B#update_block.object_type))?B,
               (update_flags(B#update_block.update_flags))?B,
               (B#update_block.movement_flags)?L,
               (B#update_block.unknown)?W,
               (B#update_block.game_time)?L,
               X?f, Y?f, Z?f, O?f,
               (B#update_block.fall_time)?L,
               W?f, R?f, WB?f, S?f, SB?f, F?f, FB?f, T?f, P?f,
               (size(B#update_block.mask) div 4)?B,
               (B#update_block.mask)/binary,
               (B#update_block.fields)/binary>>,
    packets(Rest, <<Result/binary, Binary/binary>>).

%% @spec message(binary()) -> {pid(), atom(), binary()}.
message(Packet) ->
    S = size(Packet),
    if S > 5000 ->
        Compressed = compress(Packet),
        {self(), smsg_compressed_update_object, Compressed};
    true ->
        {self(), smsg_update_object, Packet}
    end.

%% @spec compress(binary()) -> binary().
compress(Packet) ->
    Z  = zlib:open(),
    ok = zlib:deflateInit(Z, best_speed),
    P  = zlib:deflate(Z, Packet),
    L  = zlib:deflate(Z, [], finish),
    ok = zlib:deflateEnd(Z),
    zlib:close(Z),
    list_to_binary([P|L]).

%% @spec update_type(atom()) -> int().
update_type(values)         -> 0;
update_type(movement)       -> 1;
update_type(create_object)  -> 2;
update_type(create_object2) -> 3;
update_type(out_of_range)   -> 4;
update_type(in_range)       -> 5.

%% @spec update_flag(atom()) -> int().
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

%% @spec movement_flag(atom()) -> int().
movement_flag(forward)      -> 16#00000001;
movement_flag(backward)     -> 16#00000002;
movement_flag(strafe_left)  -> 16#00000004;
movement_flag(strafe_right) -> 16#00000008;
movement_flag(left)         -> 16#00000010;
movement_flag(right)        -> 16#00000020;
movement_flag(pitch_up)     -> 16#00000040;
movement_flag(pitch_down)   -> 16#00000080;
movement_flag(walk_mode)    -> 16#00000100;
movement_flag(on_transport) -> 16#00000200;
movement_flag(levitating)   -> 16#00000400;
movement_flag(fly_unk1)     -> 16#00000800;
movement_flag(jumping)      -> 16#00001000;
movement_flag(unk4)         -> 16#00002000;
movement_flag(falling)      -> 16#00004000;
movement_flag(swimming)     -> 16#00200000;
movement_flag(fly_up)       -> 16#00400000;
movement_flag(can_fly)      -> 16#00800000;
movement_flag(flying)       -> 16#01000000;
movement_flag(flying2)      -> 16#02000000;
movement_flag(spline)       -> 16#04000000;
movement_flag(spline2)      -> 16#08000000;
movement_flag(waterwalking) -> 16#10000000;
movement_flag(safe_fall)    -> 16#20000000;
movement_flag(unk3)         -> 16#40000000.

%% @spec typeid(atom()) -> int().
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

%% update_flags(list()) -> int().
update_flags(List) ->
    update_flags(List, 0).

%% update_flags(list(), int()) -> int().
update_flags([], Flags) ->
    Flags;
update_flags([Flag|Rest], Flags) ->
    update_flags(Rest, Flags bor update_flag(Flag)).

%% movement_flags(list()) -> int().
movement_flags(List) ->
    movement_flags(List, 0).

%% movement_flags(list(), int()) -> int().
movement_flags([], Flags) ->
    Flags;
movement_flags([Flag|Rest], Flags) ->
    movement_flags(Rest, Flags bor movement_flag(Flag)).

%% guid(int(), int()) -> int().
guid(HG, LG) ->
    <<255?B, HG?L, LG?L>>.
