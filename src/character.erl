-module(character).
-export([init/1]).

-include("realm_records.hrl").
-include("database_records.hrl").

% network defines
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

init(State) ->
    not_in_world(State).

not_in_world(#client_state{receiver=R, sender=S}=State) ->
    receive
    {R, cmsg_ping, D} ->
        {Sequence, Latency} = realm_patterns:cmsg_ping(D),
        S ! {self(), smsg_pong, <<Sequence?L?IN>>},
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
        in_world(State, Char);
    
    {R, Handler, Data} ->
        io:format("unhandled: ~p~n~p~n", [Handler, Data]),
        not_in_world(State);
    
    Any ->
        io:format("unauthorized: ~p~n", [Any]),
        not_in_world(State)
    end.

in_world(#client_state{receiver=R, sender=S}=State, _Char) ->
    receive
    {R, cmsg_ping, D} ->
        {Sequence, Latency} = realm_patterns:cmsg_ping(D),
        S ! {self(), smsg_pong, <<Sequence?L?IN>>},
        not_in_world(State#client_state{latency=Latency});
    
    {R, Handler, Data} ->
        io:format("unhandled: ~p~n~p~n", [Handler, Data]),
        not_in_world(State);
    
    Any ->
        io:format("unauthorized: ~p~n", [Any]),
        not_in_world(State)
    end.

set_dungeon_difficulty(S, -1) ->
    S ! {self(), msg_set_dungeon_difficulty, <<0?L, 1?L?IN, 0?L>>},
    ok;
set_dungeon_difficulty(S, Difficulty) ->
    S ! {self(), msg_set_dungeon_difficulty, <<Difficulty?L?IN>>},
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
    GameTime = common_helper:game_time(common_helper:now()),
    S ! {self(), smsg_set_rest_start, <<GameTime?L?IN>>},
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
    S ! {self(), smsg_initialize_factions, <<128?L?IN, 0:5120>>},
    ok.

send_timespeed(S) ->
    GameTime = common_helper:game_time(common_helper:now()),
    S ! {self(), smsg_login_settimespeed, <<GameTime?L?IN, (0.0166666669777748)?f>>},
    ok.

send_status(S) ->
    S ! {self(), smsg_feature_system_status, <<2?B, 0?B>>},
    ok.

send_self(S, Char) ->
    GameTime = common_helper:game_time(common_helper:now()),
    Update = <<1?L,                      % blocks count
               3?B,                      % create self
                
               255?B,                    % guid packing mask
               (Char#char.id)?L?IN, 0?L, % player guid
               4?B,                      % object type player

               16#61?B,                  % update flags
               0?L,                      % move flags
               0?W,                      % unknown flags

               GameTime?L?IN,            % current time

               (Char#char.position_x)?f, % position x
               (Char#char.position_y)?f, % position y
               (Char#char.position_z)?f, % position z
               (Char#char.orientation)?f,% orientation

               0?L,                      % unknown

               2.5?f,                    % walk speed
               2.5?f,                    % walk back speed
               7?f,                      % run speed
               4.5?f,                    % run back speed
               4.722222?f,               % swim speed
               2.5?f,                    % swim back speed
               7?f,                      % fly speed
               4.5?f,                    % fly back speed
               3.141593?f                % turn speed
               >>,
    S ! {self(), smsg_update_object, Update},
    ok.
