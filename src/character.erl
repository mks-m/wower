-module(character).
-export([init/1]).

-include("realm_records.hrl").
-include("database_records.hrl").

init(State) ->
    not_in_world(State).

not_in_world(#client_state{receiver=R, sender=S}=State) ->
    receive
    {R, cmsg_ping, D} ->
        {Sequence, Latency} = realm_patterns:cmsg_ping(D),
        {Header, Data}   = realm_patterns:smsg_pong(Sequence),
        S ! {self(), Header, Data},
        not_in_world(State#client_state{latency=Latency});
        
    {R, cmsg_char_enum, _} ->
        {Header, Data} = realm_patterns:smsg_char_enum(State#client_state.account, State#client_state.realm),
        S ! {self(), Header, Data},
        not_in_world(State);
    
    {R, cmsg_realm_split, _} ->
        {Header, Data} = realm_patterns:smsg_realm_split(),
        S ! {self(), Header, Data},
        not_in_world(State);
    
    {R, cmsg_whois, _} ->
        Whois = account_helper:get_whois(State#client_state.account),
        {Header, Data} = realm_patterns:smsg_whois(Whois),
        S ! {self(), Header, Data},
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
        in_world(State, Char);
    
    {R, Handler, Data} ->
        io:format("unhandled: ~p~n~p~n", [Handler, Data]),
        not_in_world(State);
    
    Any ->
        io:format("unauthorized: ~p~n", [Any]),
        not_in_world(State)
    end.

in_world(State, Char) ->
    ok.

set_dungeon_difficulty(S, Difficulty) ->
    {Header, Data} = realm_patterns:msg_set_dungeon_difficulty(Difficulty),
    S ! {self(), Header, Data},
    ok.

verify_world(S, Char) ->
    {Header, Data} = realm_patterns:smsg_login_verify_world(Char#char.map_id,
                                                            Char#char.position_x,
                                                            Char#char.position_y,
                                                            Char#char.position_z,
                                                            Char#char.orientation),
    S ! {self(), Header, Data},
    ok.

send_account_data(S) ->
    {Header, Data} = realm_patterns:smsg_account_data_times(),
    S ! {self(), Header, Data},
    ok.