-module(character).
-export([init/1,
         cmsg_char_enum/3,
         cmsg_char_create/3]).

-include("realm_records.hrl").
-include("database_records.hrl").
-include("dbc_records.hrl").
-include("more_records.hrl").
-include("common.hrl").

-import(packet_helper, [make_cstring/1,
                        read_cstring/1]).
-import(common_helper, [do/1]).

init(State) ->
    not_in_world(State).

not_in_world(#client_state{receiver=R, sender=S}=State) ->
    receive
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
        MapPid = verify_world(S, Char),
        ok = send_account_data(S),
        ok = set_rest_start(S),
        ok = set_tutorial_flags(S),
        ok = send_spells_and_cooldowns(S),
        ok = send_action_buttons(S),
        ok = send_factions(S),
        ok = send_timespeed(S),
        ok = send_status(S),
        %ok = update_helper:send_self(S, Char),
        B = update_helper:block(create_self, Char),
        P = update_helper:packet([B]),
        M = update_helper:message(P),
        S ! M,
        put(tick_count, 0),
        %MapPid ! {bco, self(), #vector{x=Char#char.position_x,
        %                                y=Char#char.position_y,
        %                                z=Char#char.position_z},
        %                #vector{x=30,y=30,z=30}, {object_update, OtherUpdate}},
        MapPid ! {add, self(), Char#char.position_x,
                               Char#char.position_y,
                               Char#char.position_z},
        in_world(State#client_state{current_map=MapPid, char=Char});

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

    {R, cmsg_tutorial_flag, _} ->
        in_world(State);
    
    {R, cmsg_time_sync_resp, <<Tick?L, Time?L>>} ->
        put(tick_count, Tick),
        put(client_time, Time),
        in_world(State);

    {R, {M, F}, Data} ->
        C1 = erlang:module_loaded(M),
        C2 = erlang:function_exported(M, F, 3),
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

    logout ->
		NewState = State#client_state{logout=no, current_map = -1, char=no},
		not_in_world(NewState);
        
    {update_object, Message} -> 
        S ! {self(), smsg_update_object, Message},
        in_world(State);
        
    {damage, enviroment_damage, Type, Damage} ->
        NewState = send_enviroment_damage(S, State, Type, Damage),
        in_world(NewState);

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
    world ! {self(), find, Char#char.map_id},
    receive 
    {world, found, MapPid} ->
        MapPid
    end.

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

send_tick_count(S) ->
    S ! {self(), smsg_time_sync_req, <<(get(tick_count))?L>>}.

cmsg_char_enum(S, St, _) ->
    Data = realm_patterns:smsg_char_enum(St#client_state.account, St#client_state.realm),
    S ! {self(), smsg_char_enum, Data},
    St.

cmsg_char_create(S, #client_state{account = Account, realm = Realm} = St, D) ->
    {Name, Rest} = read_cstring(D),
    <<Race?B, Class?B, Gender?B, Skin?B,
      Face?B, HS?B, HC?B, FH?B, _?B, _/binary>> = Rest,
    RaceName    = char_helper:race(Race),
    ClassName   = char_helper:class(Class),
    CreateInfo  = content:char_create_info(RaceName, ClassName),
    GenderValue = Gender * if Race =:= 10 -> -1; true -> 1 end,
    Char = #char{id               = random:uniform(16#FFFFFFFF),
                 account_id       = Account,
                 realm_id         = Realm,
                 name             = Name,
                 race             = RaceName, 
                 gender           = char_helper:gender(Gender), 
                 class            = ClassName,
                 skin             = Skin, 
                 face             = Face, 
                 hair_style       = HS, 
                 hair_color       = HC,
                 facial_hair      = FH, 
                 level            = 1,
                 guild_id         = 0,
                 general_flags    = 16#10A00040,
                 at_login_flags   = 0,
                 faction_template = CreateInfo#char_create_info.faction_template, 
                 map_id           = CreateInfo#char_create_info.map_id, 
                 zone_id          = CreateInfo#char_create_info.zone_id, 
                 position_x       = CreateInfo#char_create_info.position_x, 
                 position_y       = CreateInfo#char_create_info.position_y, 
                 position_z       = CreateInfo#char_create_info.position_z, 
                 orientation      = CreateInfo#char_create_info.orientation, 
                 display_id       = CreateInfo#char_create_info.display_id + GenderValue, 
                 strength         = CreateInfo#char_create_info.strength, 
                 agility          = CreateInfo#char_create_info.agility,
                 stamina          = CreateInfo#char_create_info.stamina, 
                 intellect        = CreateInfo#char_create_info.intellect, 
                 spirit           = CreateInfo#char_create_info.spirit, 
                 health           = CreateInfo#char_create_info.health, 
                 mana             = CreateInfo#char_create_info.mana, 
                 focus            = CreateInfo#char_create_info.focus, 
                 power            = CreateInfo#char_create_info.power, 
                 power_type       = CreateInfo#char_create_info.power_type, 
                 intro            = CreateInfo#char_create_info.intro,
                 attack_power     = CreateInfo#char_create_info.attack_power, 
                 min_dmg          = CreateInfo#char_create_info.min_dmg, 
                 max_dmg          = CreateInfo#char_create_info.max_dmg, 
                 scale            = CreateInfo#char_create_info.scale},
    ok = mnesia:dirty_write(Char),
    S ! {self(), smsg_char_create, <<16#2f?B>>},
    St.

deal_damage(PreDamage, Char) ->
    HP = Char#char.health,
    if HP > PreDamage ->
        Damage = PreDamage;
    true -> Damage = HP
    end,
    Health = HP - Damage,
    io:format("DealDamage : damage : ~p, health : ~p~n",[Damage, Health]),
    {Damage ,Char#char{health = Health}}.

send_die(_S, State) ->
    State.

send_enviroment_damage(S, State, Type, Damage) ->
    DmgType = enviroment_damage_type(Type),
    {Dmg, Char} = deal_damage(Damage, State#client_state.char),
    Data = realm_patterns:send_enviroment_damage((State#client_state.char)#char.id,
                                                    DmgType, Dmg),
    S ! {self(), smsg_environmentaldamagelog, Data},
    State#client_state{char = Char}.
    
enviroment_damage_type(exhausted) ->    0;
enviroment_damage_type(drowning) ->     1;
enviroment_damage_type(fall) ->         2;
enviroment_damage_type(lava) ->         3;
enviroment_damage_type(slime) ->        4;
enviroment_damage_type(fire) ->         5;
enviroment_damage_type(fall_to_void) -> 6.