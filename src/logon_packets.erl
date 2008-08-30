-module(logon_packets).
-compile(export_all).

-define(IN, /unsigned-little-integer).
-define(NI, /unsigned-big-integer).
-define(b,  /bytes).
-define(QQ, :256).
-define(SH, :160).
-define(DQ, :128).
-define(Q,   :64).
-define(L,   :32).
-define(W,   :16).
-define(B,    :8).

-include("logon_records.hrl").
-include("C:\\Program Files\\erl5.6.3\\lib\\stdlib-1.15.3\\include\\qlc.hrl").

dispatch(Data, State) ->
    <<Opcode?B, Rest/binary>> = Data,
    Handler = logon_opcodes:get(Opcode),
    ?MODULE:Handler(Rest, State).

%%
%% authenticate can receive only auth_request packet
%% and will try to find account and generate 
%% authentication hash for connection
%% will switch to decoder if such hash generated
%%
authenticate(Data, State) ->
        case logon_patterns:auth_request(Data) of
        {ok, Account} ->
            case mnesia:dirty_read({account, Account}) of
            [] -> 
                {send, logon_patterns:error(error_account_missing), State};
            [AccountRecord] -> 
                H = srp6:challenge(AccountRecord),
                NewState = State#logon_state{last_activity=authenticate, account=AccountRecord, hash=H},
                {send, logon_patterns:auth_reply(H), NewState};
            _ ->
                {send, logon_patterns:error(error_account_missing), State}
            end;
        _ ->
            {skip, wrong_packet(authenticate, Data), State}
        end.

%%
%% proof the challenge from receiver routine
%% back to receiver if unknown packet or wrong
%% account / password / whatever
%%
proof(Data, State) ->
        case logon_patterns:auth_proof(Data) of
        {ok, {A, M}} ->
            H = srp6:proof(A, State#logon_state.hash, State#logon_state.account),
            case H#hash.client_proof of
                M ->
                    {send, logon_patterns:auth_reproof(H), State};
                _ ->
                    {send, logon_patterns:error(error_account_missing), State}
            end;
        _ ->
            {skip, wrong_packet(proof, Data), State}
        end.

realmlist(Data, State) ->
        case logon_patterns:realmlist_request(Data) of
        {ok} ->
            GetRealms        = fun() -> qlc:eval(qlc:q([X || X <- mnesia:table(realm)])) end,
            {atomic, Realms} = mnesia:transaction(GetRealms),
            Response         = logon_patterns:realmlist_reply(Realms),
            {send, Response, State};
        _    ->
            {skip, wrong_packet(realmlist, Data), State}
        end.

wrong_packet(Handler, Data) ->
    io:format("wrong packet for ~p :~n~p", [Handler, Data]),
    ok.
