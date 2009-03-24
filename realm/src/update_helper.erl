-module(update_helper).
-export([create_update/2]).
-include("common.hrl").
-include("database_records.hrl").

%create_update(Who, Char) when is_integer(Who) and Who < 256 ->
create_update(Who, Char) ->
    Fields   = update_fields:create(player),
    GameTime = common_helper:ms_time(),
    UB  = char_helper:unit_bytes_0(Char),
    F1  = update_fields:seti(player, bytes_0, Fields, UB),
    PB1 = char_helper:player_bytes(Char),
    F2  = update_fields:seti(player, player_bytes, F1, UB),
    PB2 = char_helper:player_bytes_2(Char),
    _F3  = update_fields:seti(player, player_bytes_2, F2, UB),
    BitMask = update_fields:mask([{object, guid},
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
    Update = <<1?L,                      % blocks count
               Who?B,                    % who create
                
               255?B,                    % guid packing mask
               (Char#char.id)?L, 0?L,    % player guid
               4?B,                      % object type player

               (64 bor 32 bor 1)?B,      % update flags
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
               >>,
    Update.