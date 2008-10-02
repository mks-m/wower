-module(realm_packets).
-export([dispatch/2, send/3, skip/1,
         cmsg_auth_session/2,
         cmsg_char_enum/2]).

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
    io:format("[e] handling: ~p~n", [Handler]),
    ?MODULE:Handler(Data, State#client_state{key = NewKey}).

cmsg_auth_session(Rest, State) ->
    {_, A, _}      = realm_patterns:cmsg_auth_session(Rest),
    {Header, Data} = realm_patterns:smsg_auth_response(),
    K      = realm_crypto:encryption_key(A),
    Crypt  = #crypt_state{si=0, sj=0, ri=0, rj=0, key=K},
    send(Header, Data, State#client_state{key=Crypt, account=A}).

cmsg_char_enum(_, State) ->
    {Header, Data} = realm_patterns:smsg_char_enum(State#client_state.account),
    send(Header, Data, State).

send(Header, Data, #client_state{key = Crypt} = State) ->
    {H, C} = realm_crypto:encrypt(Header, Crypt),
    {send, <<H/binary, Data/binary>>, State#client_state{key=C}}.

skip(State) ->
    {skip, undefined, State}.
