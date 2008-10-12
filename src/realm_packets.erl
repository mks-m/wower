-module(realm_packets).
-export([dispatch/2, send/3, skip/1,
         cmsg_auth_session/2,
         cmsg_char_enum/2,
         cmsg_realm_split/2,
         cmsg_ping/2,
         cmsg_whois/2,
         cmsg_voice_session_enable/2,
         msg_query_guild_bank_text/2,
         cmsg_player_login/2,
         undef_opcode/2]).

-include("realm_records.hrl").
-include("database_records.hrl").

dispatch(Data, #client_state{key=null, account=null} = State) ->
    <<_:16, Opcode:32/integer-little, Rest/binary>> = Data,
    Handler = realm_opcodes:h(Opcode),
    io:format("[d] handling: ~p~n", [Handler]),
    ?MODULE:Handler(Rest, State);
dispatch(<<Header:6/bytes, Data/binary>>, State) ->
    {DecryptedHeader, NewKey} = realm_crypto:decrypt(Header, State#client_state.key),
    <<_:16, Opcode:16/integer-little, _:16>> = DecryptedHeader,
    Handler = realm_opcodes:h(Opcode),
    io:format("[e] handling: ~p (~p)~n", [Handler, Opcode]),
    ?MODULE:Handler(Data, State#client_state{key = NewKey}).

cmsg_auth_session(Rest, State) ->
    {_, A, _}      = realm_patterns:cmsg_auth_session(Rest),
    {Header, Data} = realm_patterns:smsg_auth_response(),
    K      = realm_crypto:encryption_key(A),
    Crypt  = #crypt_state{si=0, sj=0, ri=0, rj=0, key=K},
    [Account] = account_helper:find_by_name(A),
    send(Header, Data, State#client_state{key=Crypt, account=Account#account.id}).

cmsg_char_enum(_, State) ->
    {Header, Data} = realm_patterns:smsg_char_enum(State#client_state.account,
                                                   State#client_state.realm),
    send(Header, Data, State).

cmsg_realm_split(_, State) ->
    {Header, Data} = realm_patterns:smsg_realm_split(),
    send(Header, Data, State).

cmsg_ping(Binary, State) ->
    {Sequence, Latency} = realm_patterns:cmsg_ping(Binary),
    {Header, Data} = realm_patterns:smsg_pong(Sequence),
    send(Header, Data, State#client_state{latency=Latency}).

cmsg_whois(_, State) ->
    Whois = account_helper:get_whois(State#client_state.account),
    {Header, Data} = realm_patterns:smsg_whois(Whois),
    send(Header, Data, State).

cmsg_voice_session_enable(_, State) ->
    skip(State).

msg_query_guild_bank_text(_, State) ->
    skip(State).

cmsg_player_login(_, State) ->
    skip(State).

undef_opcode(Binary, State) ->
    io:format("~p~n", [Binary]),
    skip(State).

send(Header, Data, #client_state{key = Crypt} = State) ->
    {H, C} = realm_crypto:encrypt(Header, Crypt),
    {send, <<H/binary, Data/binary>>, State#client_state{key=C}}.

skip(State) ->
    {skip, undefined, State}.
