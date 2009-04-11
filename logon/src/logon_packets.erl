-module(logon_packets).
-compile(export_all).

-include("logon_records.hrl").
-include("database_records.hrl").
-include_lib("stdlib/include/qlc.hrl").

%%
%% authenticate can receive only auth_request packet
%% and will try to find account and generate 
%% authentication hash for connection
%% will switch to decoder if such hash generated
%% 
%% @spec authenticate(atom(), binary(), tuple()) -> {send, binary(), tuple()}.
authenticate(Opcode, Data, State) ->
    case logon_patterns:auth_request(Data) of
    {ok, Build, Account} when Build > 9182 ->
        case account_helper:find_by_name(Account) of
        {ok, AccountRecord} ->
            H = srp6:challenge(AccountRecord),
            NewState = State#logon_state{authenticated=no, account=AccountRecord, hash=H},
            {send, logon_patterns:auth_reply(H), NewState};
        {error, not_found} ->
            {send, logon_patterns:error(Opcode, account_missing), State}
        end;
    {ok, _Build, _Account} ->
        {send, logon_patterns:error(Opcode, wrong_build), State};
    _ ->
        wrong_packet(authenticate, Data),
        {send, logon_patterns:error(Opcode, account_missing), State}
    end.

%%
%% proof the challenge from receiver routine
%% back to receiver if unknown packet or wrong
%% account / password / whatever
%%
%% @spec proof(atom(), binary(), tuple()) -> {send, binary(), tuple()} | {skip, ok, tuple()}.
proof(Opcode, Data, State) ->
    case logon_patterns:auth_proof(Data) of
    {ok, {A, M}} ->
        H = srp6:proof(A, State#logon_state.hash, State#logon_state.account),
        case H#hash.client_proof of
        M ->
            %% client accepted and authenticated, we must store his session key for
            %% later realm_server authentication
            ets:insert(connected_clients, {(State#logon_state.account)#account.name, H#hash.session_key}),
            {send, logon_patterns:auth_reproof(H), State#logon_state{authenticated=yes}};
        _ ->
            {send, logon_patterns:error(Opcode, account_missing), State}
        end;
    _ ->
        {skip, wrong_packet(proof, Data), State}
    end.

%% @spec realmlist(atom(), binary(), tuple()) -> {send, binary(), tuple()} | {skip, ok, tuple()}.
realmlist(_Opcode, Data, #logon_state{authenticated=yes} = State) ->
    case logon_patterns:realmlist_request(Data) of
    {ok} ->
        Realms   = realm_helper:realms(),
        AccData  = lists:map(fun(E) -> realm_helper:number_of_chars((State#logon_state.account)#account.id, E#realm.id) end, Realms),
        Response = logon_patterns:realmlist_reply(Realms, AccData),
        {send, Response, State};
    _    ->
        {skip, wrong_packet(realmlist, Data), State}
    end;
realmlist(Opcode, _, State) ->
    {send, logon_patterns:error(Opcode, acount_missing), State}.

%% @spec wrong_opcode(atom, binary(), tuple()) -> {skip, ok, tuple()}.
wrong_opcode(Opcode, _, State) ->
    io:format("unimplemented opcode ~p~n", [Opcode]),
    {skip, ok, State}.

%% @spec wrong_packet(atom(), binary()) -> ok.
wrong_packet(Handler, Data) ->
    io:format("wrong packet for ~p :~n~p", [Handler, Data]),
    ok.

%% @spec wrong_code(int()) -> ok.
wrong_code(Error) ->
    io:format("got error: ~p~n", [Error]),
    ok.
