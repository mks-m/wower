-module(logon_packets).
-export([receiver/2, list/0]).

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
-include_lib("stdlib/include/qlc.hrl").
-define(CHECK, io:format("check~n", [])).

error(C) when atom(C) ->
    <<0?B, 0?B, (logon_opcodes:get(C))?B>>;
error(C) when integer(C) ->
    <<0?B, 0?B, C?B>>;
error(C) ->
    io:format("wrong errorcode: ~p~n", [C]),
    <<0?B, 0?B, 1?B>>.

%%
%% receiver can receive only auth_request packet
%% and will try to find account and generate 
%% authentication hash for connection
%% will switch to decoder if such hash generated
%%
receiver(Socket, Pid) ->
    case gen_tcp:recv(Socket, 0) of
    {ok, Data} ->
        case logon_patterns:auth_request(Data) of
        {ok, Account} ->
            case mnesia:dirty_read({account, Account}) of
            [] -> 
                gen_tcp:send(Socket, error(error_account_missing)),
                receiver(Socket, Pid);
            [AccountRecord] -> 
                H = srp6:challenge(AccountRecord),
                gen_tcp:send(Socket, logon_patterns:auth_reply(H)),
                proof(Socket, Pid, H, AccountRecord);
            _ ->
                gen_tcp:send(Socket, error(error_account_missing)),
                receiver(Socket, Pid)
            end;
        _ ->
            receiver(Socket, Pid)
        end;
    {error, closed} ->
        close()
    end.

%%
%% proof the challenge from receiver routine
%% back to receiver if unknown packet or wrong
%% account / password / whatever
%%
proof(Socket, Pid, Hash, Account) ->
    case gen_tcp:recv(Socket, 0) of
    {ok, Data} ->
        case logon_patterns:auth_proof(Data) of
        {ok, {A, M}} ->
            H = srp6:proof(A, Hash, Account),
            case H#hash.client_proof of
                M ->
                    gen_tcp:send(Socket, logon_patterns:auth_reproof(H)),
                    realmlist(Socket, Pid, H, Account);
                _ ->
                    receiver(Socket, Pid)
            end;
        _ ->
            io:format("unknown packet: ~p~n", [Data]),
            receiver(Socket, Pid)
        end;
    {error, closed} ->
        close()
    end.

realmlist(Socket, Pid, Hash, Account) ->
    case gen_tcp:recv(Socket, 0) of
    {ok, Data} ->
        case logon_patterns:realmlist_request(Data) of
        {ok} ->
            GetRealms        = fun() -> qlc:eval(qlc:q([X || X <- mnesia:table(realm)])) end,
            {atomic, Realms} = mnesia:transaction(GetRealms),
            Response         = logon_patterns:realmlist_reply(Realms),
            io:format("~n~n~p~n~n", [Response]),
            gen_tcp:send(Socket, Response),
            realmlist(Socket, Pid, Hash, Account);
        _    ->
            io:format("unknown packet: ~p~n", [Data]),
            receiver(Socket, Pid)
        end,
        realmlist(Socket, Pid, Hash, Account);
    {error, closed} ->
        close()
    end.

list() ->
    GetRealms = fun() -> qlc:eval(qlc:q([X || X <- mnesia:table(realm)])) end,
    mnesia:transaction(GetRealms).

close() ->
    io:format("  client socket closed~n", []),
    ok.
