%% Author: kp
%% Created: Jan 11, 2009
%% Description: movement flags and functions
-module(movement_helper).

-export([info/1]).

-include("realm_records.hrl").

% network defines
-define(I, /unsigned-little-integer).
-define(O, /unsigned-big-integer).
-define(b,  /bytes).                   % that's for plain text
-define(f,  :32/float-little).         % float values obviously
-define(SH, :160?I).                   % SH is for sha1
-define(Q,   :64?I).                   % uint64 - Q for quad
-define(L,   :32?I).                   % uint32 - L for long
-define(W,   :16?I).                   % uint16 - W for word
-define(B,    :8).                     % byte (doesn't need to be endianated)

-define(forward,      16#00000001).
-define(backward,     16#00000002).
-define(strafe_left,  16#00000004).
-define(strafe_right, 16#00000008).
-define(left,         16#00000010).
-define(right,        16#00000020).
-define(pitch_up,     16#00000040).
-define(pitch_down,   16#00000080).
-define(walk_mode,    16#00000100).
-define(on_transport, 16#00000200).
-define(levitating,   16#00000400).
-define(fly_unk1,     16#00000800).
-define(jumping,      16#00001000).
-define(unk4,         16#00002000).
-define(falling,      16#00004000).
-define(swimming,     16#00200000).
-define(fly_up,       16#00400000).
-define(can_fly,      16#00800000).
-define(flying,       16#01000000).
-define(flying2,      16#02000000).
-define(spline,       16#04000000).
-define(spline2,      16#08000000).
-define(waterwalking, 16#10000000).
-define(safe_fall,    16#20000000).
-define(unk3,         16#40000000).

info(<<Flags?L, Unk1?W, Time?L, X?f, Y?f, Z?f, O?f, Rest/binary>>) ->
    MI = #movement_info{flags = Flags, unk1 = Unk1, time = Time,
                        x = X, y = Y, z = Z, o = O},
    if (Flags band ?on_transport) > 0 ->
        <<T_guid?Q, Tx?f, Ty?f, Tz?f, 
          To?f, T_time?L, T_seat?B, Rest2/binary>> = Rest,
        MI2 = MI#movement_info{t_guid = T_guid, tx = Tx, ty = Ty, tz = Tz, 
                               to = To, t_time = T_time, t_seat = T_seat};
    true ->
        MI2 = MI, Rest2 = Rest
    end,

    if ((Flags band (?swimming bor ?flying2)) > 0) or 
       ((Unk1 band 16#20) > 0) ->
        <<S_pitch?f, Rest3/binary>> = Rest2,
        MI3 = MI2#movement_info{s_pitch = S_pitch};
    true ->
        MI3 = MI2, Rest3 = Rest2
    end,

    <<Fall_time?L, Rest4/binary>> = Rest3,
    MI4 = MI3#movement_info{fall_time = Fall_time},

    if (Flags band ?jumping) > 0 ->
        <<Unk2?f, J_sin?f, J_cos?f, J_speed?f, Rest5/binary>> = Rest4,
        MI5 = MI4#movement_info{unk2 = Unk2, j_sin = J_sin, 
                               j_cos = J_cos, j_speed = J_speed};
    true ->
        MI5 = MI4, Rest5 = Rest4
    end,

    if (Flags band ?spline) > 0 ->
        <<Unk3?f, _/binary>> = Rest5,
        MI6 = MI5#movement_info{unk3 = Unk3};
    true ->
        MI6 = MI5
    end,

    {ok, MI6};
info(_) ->
    {error, not_movement_info}.
