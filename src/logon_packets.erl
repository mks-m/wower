-module(logon_packets).
-export([receiver/2, encoder/1]).

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
                H = hash(AccountRecord),
                Response = logon_patterns:auth_reply(H),
                gen_tcp:send(Socket, Response),
                handshaker(Socket, Pid, H, AccountRecord);
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

handshaker(Socket, Pid, Hash, Account) ->
    case gen_tcp:recv(Socket, 0) of
    {ok, Data} ->
        case logon_patterns:auth_proof(Data) of
        {ok, {A, M}} ->
            proof(A, M, Hash, Account),
            decoder(Socket, Pid, Hash);
        _ ->
            io:format("unknown packet: ~p~n", [Data]),
            handshaker(Socket, Pid, Hash, Account)
        end;
    {error, closed} ->
        close()
    end.

decoder(Socket, Pid, Hash) ->
    case gen_tcp:recv(Socket, 0) of
    {ok, _Data} ->
        decoder(Socket, Pid, Hash);
    {error, closed} ->
        close()
    end.

encoder(Hash) ->
    Hash.

hash(Account) ->
    srp6:challenge(Account).

proof(A, M, H, P) ->
    M2 = srp6:proof(A, H, P),
    io:format("M values should match~n~.16B~n~.16B~n", [M, M2]).

close() ->
    io:format("  client socket closed~n", []),
    ok.
