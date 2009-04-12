%% Author: kp
%% Created: Jan 11, 2009
%% Description: movement flags and functions
-module(movement).

-export([info/1, start_forward/3, start_backward/3,
         heartbeat/3, start_turn_right/3, start_turn_left/3,
         stop_turn/3, stop/3, fall_land/3, set_facing/3]).

-include("realm_records.hrl").
-include("database_records.hrl").
-include("common.hrl").

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

%% @spec info(binary()) -> {error, not_movement_info} | tuple().
info(<<Flags?L, Unk1?W, Time?L, X?f, Y?f, Z?f, O?f, Rest/binary>>) ->
    MI = #movement_info{flags = Flags, unk1 = Unk1, time = Time,
                        x = X, y = Y, z = Z, o = O},
    %% io:format("mi1: ~p~n", [MI]),
    if (Flags band ?on_transport) > 0 ->
        <<T_guid?Q, Tx?f, Ty?f, Tz?f, 
          To?f, T_time?L, T_seat?B, Rest2/binary>> = Rest,
        MI2 = MI#movement_info{t_guid = T_guid, tx = Tx, ty = Ty, tz = Tz, 
                               to = To, t_time = T_time, t_seat = T_seat};
    true ->
        MI2 = MI, Rest2 = Rest
    end,
    %% io:format("mi2: ~p~n", [MI2]),

    if ((Flags band (?swimming bor ?flying2)) > 0) or 
       ((Unk1 band 16#20) > 0) ->
        <<S_pitch?f, Rest3/binary>> = Rest2,
        MI3 = MI2#movement_info{s_pitch = S_pitch};
    true ->
        MI3 = MI2, Rest3 = Rest2
    end,
    %% io:format("mi3: ~p~n", [MI2]),

    <<Fall_time?L, Rest4/binary>> = Rest3,
    MI4 = MI3#movement_info{fall_time = Fall_time},
    %% io:format("mi4: ~p~n", [MI2]),

    if (Flags band ?jumping) > 0 ->
        <<Unk2?f, J_sin?f, J_cos?f, J_speed?f, Rest5/binary>> = Rest4,
        MI5 = MI4#movement_info{unk2 = Unk2, j_sin = J_sin, 
                               j_cos = J_cos, j_speed = J_speed};
    true ->
        MI5 = MI4, Rest5 = Rest4
    end,
    %% io:format("mi5: ~p~n", [MI2]),

    if (Flags band ?spline) > 0 ->
        <<Unk3?f, _/binary>> = Rest5,
        MI6 = MI5#movement_info{unk3 = Unk3};
    true ->
        MI6 = MI5
    end,
    %% io:format("mi6: ~p~n", [MI2]),

    {ok, MI6};
info(_) ->
    {error, not_movement_info}.

%% @spec start_forward(pid(), tuple(), binary()) -> tuple().
start_forward(_S, State, Data) ->
    ?DINFO("forward~n"),
    movement(State, Data).

%% @spec start_backward(pid(), tuple(), binary()) -> tuple().
start_backward(_S, State, Data) ->
    ?DINFO("backward~n"),
    movement(State, Data).

%% @spec heartbeat(pid(), tuple(), binary()) -> tuple().
heartbeat(_S, State, Data) ->
    ?DINFO("heartbeat~n"),
    movement(State, Data),
    State.

%% @spec start_turn_left(pid(), tuple(), binary()) -> tuple().
start_turn_left(_S, State, Data) ->
    ?DINFO("turn left~n"),
    movement(State, Data).

%% @spec start_turn_right(pid(), tuple(), binary()) -> tuple().
start_turn_right(_S, State, Data) ->
    ?DINFO("turn right~n"),
    movement(State, Data).

%% @spec stop_turn(pid(), tuple(), binary()) -> tuple().
stop_turn(_S, State, Data) ->
    ?DINFO("turn stop~n"),
    movement(State, Data).

%% @spec stop(pid(), tuple(), binary()) -> tuple().
stop(_S, State, Data) ->
    ?DINFO("stop~n"),
    movement(State, Data).

%% @spec fall_land(pid(), tuple(), binary()) -> tuple().
fall_land(_S, State, Data) ->
    ?DINFO("fall_land~n"),
    movement(State, Data).

%% @spec set_facing(pid(), tuple(), binary()) -> tuple().
set_facing(_S, State, Data) ->
    ?DINFO("set_facing~n"),
    movement(State, Data).

%% @spec movement(pid(), tuple(), binary()) -> tuple().
movement(State, Data) ->
    {ok, MI} = info(Data),
    ?DEXEC(debug_mi(MI)),
    Char = (State#client_state.char)#char{position_x  = MI#movement_info.x,
                                          position_y  = MI#movement_info.y,
                                          position_z  = MI#movement_info.z,
                                          orientation = MI#movement_info.o},
    State#client_state.current_map ! {set, self(), MI#movement_info.x,
                                                   MI#movement_info.y,
                                                   MI#movement_info.z},
    State#client_state{char = Char}.

%% @spec debug_mi(tuple()) -> ok.
debug_mi(MI) ->
    lists:foldl(
        fun
        (E, I) ->
            V = element(I, MI),
            if V /= undefined ->
                io:format("  ~8s: ~p~n", [E, element(I, MI)]);
            true ->
                ok
            end,
            I + 1
        end,
        2, record_info(fields, movement_info)),
    ok.
