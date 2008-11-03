-module(character).
-export([init/1]).

-include("realm_records.hrl").
-include("database_records.hrl").

-import(packet_helper, [make_cstring/1,
                        read_cstring/1]).

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

init(State) ->
    not_in_world(State).

not_in_world(#client_state{receiver=R, sender=S}=State) ->
    receive
    {R, cmsg_ping, D} ->
        {Sequence, Latency} = realm_patterns:cmsg_ping(D),
        S ! {self(), smsg_pong, <<Sequence?L>>},
        not_in_world(State#client_state{latency=Latency});
        
    {R, cmsg_char_enum, _} ->
        Data = realm_patterns:smsg_char_enum(State#client_state.account, State#client_state.realm),
        S ! {self(), smsg_char_enum, Data},
        not_in_world(State);
    
    {R, cmsg_realm_split, _} ->
        Date = list_to_binary("01/01/01"),
        S ! {self(), smsg_realm_split, <<16#FFFFFFFF?L, 0?L, Date/binary, 0?B>>},
        not_in_world(State);
    
    {R, cmsg_whois, _} ->
        Whois = account_helper:get_whois(State#client_state.account),
        S ! {self(), smsg_whois, <<Whois/bytes, 0?B>>},
        not_in_world(State);
    
    {R, cmsg_voice_session_enable, _} ->
        not_in_world(State);
    
    {R, msg_query_guild_bank_text, _} ->
        not_in_world(State);
    
    {R, cmsg_player_login, D} ->
        {ok, CharId} = realm_patterns:cmsg_player_login(D),
        Char = char_helper:find(CharId),
        ok = set_dungeon_difficulty(S, -1),
        ok = verify_world(S, Char),
        ok = send_account_data(S),
        ok = set_rest_start(S),
        ok = set_tutorial_flags(S),
        ok = send_spells_and_cooldowns(S),
        ok = send_action_buttons(S),
        ok = send_factions(S),
        ok = send_timespeed(S),
        ok = send_status(S),
        ok = send_self(S, Char),
        put(tick_count, 0),
        io:format("entering world loop~n"),
        in_world(State#client_state{char=Char});

    {R, {M, F}, Data} ->
        C1 = erlang:module_loaded(M),
        C2 = erlang:function_exported(M, F, 3),
        if C1 and C2 ->
            NewState = M:F(S, State, Data),
            not_in_world(NewState);
        true ->
            io:format("undefined: ~p:~p(~p)~n", [M, F, Data]),
            not_in_world(State)
        end;
    
    {R, Handler, Data} ->
        io:format("unhandled: ~p(~p)~n", [Handler, Data]),
        not_in_world(State);
    
    Any ->
        io:format("unauthorized: ~p~n", [Any]),
        not_in_world(State)
    end.

in_world(#client_state{receiver=R, sender=S, char=Char}=State) ->
    receive
    {R, cmsg_ping, D} ->
        {Sequence, Latency} = realm_patterns:cmsg_ping(D),
        S ! {self(), smsg_pong, <<Sequence?L>>},
        in_world(State#client_state{latency=Latency});
    
    {R, cmsg_name_query, _} ->
        Response = <<(Char#char.id)?L, 0?L,
                     (make_cstring(Char#char.name))/binary,
                     0?B, 1?B, 1?B, 1?B, 0?B>>,
        S ! {self(), smsg_name_query_response, Response},
        in_world(State);
    
    {R, cmsg_update_account_data, _} ->
        in_world(State);
    
    {R, cmsg_set_active_mover, <<Mover?Q>>} ->
        put(active_mover, Mover),
        send_tick_count(S),
        in_world(State);
    
    {R, cmsg_set_actionbar_toggles, _} ->
        in_world(State);
    
    {R, cmsg_request_raid_info, _} ->
        S ! {self(), smsg_raid_instance_info, <<0?L>>},
        in_world(State);
    
    {R, cmsg_gmticket_getticket, _} ->
        S ! {self(), smsg_gmticket_getticket, <<10?L>>},
        in_world(State);
    
    {R, cmsg_query_time, _} ->
        UnixTime = common_helper:unix_time(),
        S ! {self(), smsg_query_time_response, <<UnixTime?L>>},
        in_world(State);
    
    {R, cmsg_item_query_single, _} ->
        in_world(State);

    {R, msg_query_next_mail_time, _} ->
        S ! {self(), msg_query_next_mail_time, <<16#C7A8C000?L, 0?L>>},
        in_world(State);

    {R, cmsg_battlefield_status, _} ->
        in_world(State);

    {R, cmsg_meetingstone_info, _} ->
        S ! {self(), smsg_meetingstone_setqueue, <<0?L, 6?B>>},
        in_world(State);

    {R, msg_guild_event_log_query, _} ->
        S ! {self(), msg_guild_event_log_query, <<0?B>>},
        in_world(State);

    {R, cmsg_move_time_skipped, _} ->
        in_world(State);

    {R, msg_move_fall_land, _} ->
        in_world(State);

    {R, cmsg_join_channel, _} ->
        in_world(State);

    {R, cmsg_tutorial_flag, _} ->
        in_world(State);
    
    {R, cmsg_time_sync_resp, <<Tick?L, Time?L>>} ->
        put(tick_count, Tick),
        put(client_time, Time),
        in_world(State);

    {R, {M, F}, Data} ->
        C1 = erlang:module_loaded(M),
        C2 = erlang:function_exported(M, F, 4),
        if C1 and C2 ->
            NewState = M:F(S, State, Data),
            in_world(NewState);
        true ->
            io:format("undefined: ~p:~p(~p)~n", [M, F, Data]),
            in_world(State)
        end;
    
    {R, Handler, Data} ->
        io:format("unhandled: ~p(~p)~n", [Handler, Data]),
        in_world(State);
    
    Any ->
        io:format("unauthorized: ~p~n", [Any]),
        in_world(State)
    end.

set_dungeon_difficulty(S, -1) ->
    S ! {self(), msg_set_dungeon_difficulty, <<0?L, 1?L, 0?L>>},
    ok;
set_dungeon_difficulty(S, Difficulty) ->
    S ! {self(), msg_set_dungeon_difficulty, <<Difficulty?L>>},
    ok.

verify_world(S, Char) ->
    S ! { self(), smsg_login_verify_world, 
          <<(Char#char.map_id)?f, (Char#char.position_x)?f, 
            (Char#char.position_y)?f, (Char#char.position_z)?f,
            (Char#char.orientation)?f>> },
    ok.

send_account_data(S) ->
    S ! {self(), smsg_account_data_times, <<0:1024>>},
    ok.

set_rest_start(S) ->
    GameTime = common_helper:game_time(),
    S ! {self(), smsg_set_rest_start, <<GameTime?L>>},
    ok.

set_tutorial_flags(S) ->
    S ! {self(), smsg_tutorial_flags, <<0:256>>},
    ok.

send_spells_and_cooldowns(S) ->
    S ! {self(), smsg_initial_spells, <<0?B, 0?W, 0?W>>},
    ok.

send_action_buttons(S) ->
    S ! {self(), smsg_action_buttons, <<>>},
    ok.

send_factions(S) ->
    S ! {self(), smsg_initialize_factions, <<128?L, 0:5120>>},
    ok.

send_timespeed(S) ->
    GameTime = common_helper:game_time(),
    S ! {self(), smsg_login_settimespeed, <<GameTime?L, (0.0166666669777748)?f>>},
    ok.

send_status(S) ->
    S ! {self(), smsg_feature_system_status, <<2?B, 0?B>>},
    ok.

send_self(S, Char) ->
    GameTime = common_helper:ms_time(),
    UB = char_helper:unit_bytes(Char),
    PB1 = char_helper:player_bytes1(Char),
    PB2 = char_helper:player_bytes2(Char),
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
               3?B,                      % create self
                
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
               1.0?f,                    % scale
               UB/binary,                % race, class, gender, power
               1000?L,                   % health
               1000?L,                   % max health
               (Char#char.level)?L,
               4?L,                      % factiontemplate
               60?L,                     % display ID
               0?L,                      % dynamic flag (0 = alive)
               PB1/binary,               % skin, face, hair style, hair color
               PB2/binary                % facial hair, unknown
               >>,
    S ! {self(), smsg_update_object, Update},
    ok.

send_tick_count(S) ->
    S ! {self(), smsg_time_sync_req, <<(get(tick_count))?L>>}.
