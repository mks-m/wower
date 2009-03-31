%% Author: kp
%% Created: Jan 11, 2009
%% Description: movement flags and functions
-module(movement).

-export([movement_extract/1, start_forward/3, start_backward/3,
         heartbeat/3, start_turn_right/3, start_turn_left/3,
         stop_turn/3, stop/3, fall_land/3, jump/3]).

-include("realm_records.hrl").
-include("database_records.hrl").
-include("common.hrl").
    
movement_extract(<<Flags?L, Unk1?W, Time?L, X?f, Y?f, Z?f, O?f, Rest/binary>>) ->
    PreMI = #movement_info{flags = Flags, unk1 = Unk1, time = Time,
                        x = X, y = Y, z = Z, o = O},
    {MI, _ExtRest} = extract_on_transport(PreMI, Rest),
    {ok, MI};
    
movement_extract(_) ->
    {error, not_movement_info}.

extract_on_transport(MovInfo, Data) ->
    Flags = MovInfo#movement_info.flags,
    case (Flags band (movement_helper:flag(on_transport))) of
    true -> 
        <<T_guid?Q, Tx?f, Ty?f, Tz?f, 
        To?f, T_time?L, T_seat?B, Rest/binary>> = Data,
        extract_swimming_or_flying(MovInfo#movement_info{t_guid = T_guid, tx = Tx, ty = Ty, tz = Tz, 
                           to = To, t_time = T_time, t_seat = T_seat}, Rest);
    _ ->
        extract_swimming_or_flying(MovInfo, Data)
    end.
    
    
extract_swimming_or_flying(MovInfo, Data)->
    Flags = MovInfo#movement_info.flags,
    Unk1 = MovInfo#movement_info.unk1,
    case ((Flags band (movement_helper:flag(swimming) bor movement_helper:flag(flying2))) > 0) or ((Unk1 band 16#20) > 0) of
    true ->
        <<S_pitch?f, Rest/binary>> = Data,
        extract_fall_time(MovInfo#movement_info{s_pitch = S_pitch}, Rest);
    _ ->
        extract_fall_time(MovInfo, Data)
    end.
    
extract_fall_time(MovInfo, Data) ->
    <<Fall_time?L, Rest/binary>> = Data,
    extract_jumping(MovInfo#movement_info{fall_time = Fall_time}, Rest).

extract_jumping(MovInfo, Data) ->
    Flags = MovInfo#movement_info.flags,
    case (Flags band movement_helper:flag(jumping)) > 0 of
    true ->
        <<Unk2?f, J_sin?f, J_cos?f, J_speed?f, Rest/binary>> = Data,
        extract_spline(MovInfo#movement_info{unk2 = Unk2, j_sin = J_sin, 
                               j_cos = J_cos, j_speed = J_speed}, Rest);
    _ ->
        extract_spline(MovInfo, Data)
    end.

extract_spline(MovInfo, Data) ->
    Flags = MovInfo#movement_info.flags,
    case (Flags band movement_helper:flag(spline)) > 0 of
    true ->
        <<Unk3?f, Rest/binary>> = Data,
        {MovInfo#movement_info{unk3 = Unk3}, Rest};
    _ ->
        {MovInfo, Data}
    end.
    
start_forward(_S, State, Data) ->
    io:format("forward:~n"),
    movement(State, Data, start_forward).

start_backward(_S, State, Data) ->
    io:format("backward:~n"),
    movement(State, Data, start_backward).

heartbeat(_S, State, Data) ->
    io:format("heartbeat:~n"),
    movement(State, Data, heartbeat),
    State.

start_turn_left(_S, State, Data) ->
    io:format("turn left:~n"),
    movement(State, Data, start_turn_left).

start_turn_right(_S, State, Data) ->
    io:format("turn right:~n"),
    movement(State, Data, start_turn_right).

stop_turn(_S, State, Data) ->
    io:format("turn stop:~n"),
    movement(State, Data, stop_turn).

stop(_S, State, Data) ->
    io:format("stop:~n"),
    movement(State, Data, stop).

movement(State, Data, Handler) ->
    {ok, MI} = movement_extract(Data),
    %lists:foldl(
    %    fun
    %    (E, I) ->
    %        V = element(I, MI),
    %        if V /= undefined -> 
    %            io:format("  ~8s: ~p~n", [E, element(I, MI)]);
    %        true ->
    %            ok
    %        end,
    %        I + 1
    %    end, 
    %    2, record_info(fields, movement_info)),
    Char = (State#client_state.char)#char{position_x  = MI#movement_info.x,
                                          position_y  = MI#movement_info.y,
                                          position_z  = MI#movement_info.z,
                                          orientation = MI#movement_info.o},
    State#client_state.current_map ! {set, self(), MI#movement_info.x,
                                                   MI#movement_info.y,
                                                   MI#movement_info.z},
    if (fall_land =:= Handler) and (MI#movement_info.fall_time > 1100) ->
        %calculate damage and damage inviroment sending
        FallPerc = (MI#movement_info.fall_time) / 1100.0,
        %must be max_health, remaking need...
        PreDamage = round((FallPerc*FallPerc - 1)/9 * (State#client_state.char)#char.health),
        if PreDamage > (State#client_state.char)#char.health ->
            Damage = (State#client_state.char)#char.health;
        true -> Damage = PreDamage
        end,
        self() ! {damage, enviroment_damage, fall, Damage},
        io:format("Damage from falling taking(~p)~n", [Damage]);
    true -> ok
    end,
    State#client_state{char = Char}.

fall_land(_S, State, Data) ->
    io:format("fall_land~n"),
    movement(State, Data, fall_land).
    
jump(_S, State, Data) ->
    io:format("jump~n"),
    movement(State, Data, jump).