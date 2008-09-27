-module(logon_packets).
-compile(export_all).

-include("logon_records.hrl").
-include("database_records.hrl").
-include_lib("stdlib/include/qlc.hrl").

dispatch(Data, State) ->
    <<Opcode:8/integer, Rest/binary>> = Data,
    Handler = logon_opcodes:get(Opcode),
    io:format("handling ~p~n", [Handler]),
    ?MODULE:Handler(Opcode, Rest, State).

%%
%% authenticate can receive only auth_request packet
%% and will try to find account and generate 
%% authentication hash for connection
%% will switch to decoder if such hash generated
%%
authenticate(Opcode, Data, State) ->
    case logon_patterns:auth_request(Data) of
    {ok, Account} ->
        case mnesia:dirty_read({account, Account}) of
        [AccountRecord] -> 
            H = srp6:challenge(AccountRecord),
            NewState = State#logon_state{authenticated=no, account=AccountRecord, hash=H},
            {send, logon_patterns:auth_reply(H), NewState};
        _ ->
            {send, logon_patterns:error(Opcode, account_missing), State}
        end;
    _ ->
        wrong_packet(authenticate, Data),
        {send, logon_patterns:error(Opcode, account_missing), State}
    end.

%%
%% proof the challenge from receiver routine
%% back to receiver if unknown packet or wrong
%% account / password / whatever
%%
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

realmlist(_Opcode, Data, #logon_state{authenticated=yes} = State) ->
    case logon_patterns:realmlist_request(Data) of
    {ok} ->
        GetRealms        = fun() -> qlc:eval(qlc:q([X || X <- mnesia:table(realm)])) end,
        {atomic, Realms} = mnesia:transaction(GetRealms),
        Response         = logon_patterns:realmlist_reply(Realms),
        {send, Response, State};
    _    ->
        {skip, wrong_packet(realmlist, Data), State}
    end;
realmlist(Opcode, _, State) ->
    {send, logon_patterns:error(Opcode, acount_missing), State}.

wrong_opcode(Opcode, _, State) ->
    io:format("unimplemented opcode ~p~n", [Opcode]),
    {skip, ok, State}.

wrong_packet(Handler, Data) ->
    io:format("wrong packet for ~p :~n~p", [Handler, Data]).

wrong_code(Error) ->
    io:format("got error: ~p~n", [Error]).
