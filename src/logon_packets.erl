-module(logon_packets).
-export([receiver/2, encoder/1, even/1, odd/1]).

-define(QQ, :256/unsigned-little-integer).
-define(SH, :160/unsigned-little-integer).
-define(DQ, :128/unsigned-little-integer).
-define(Q,   :64/unsigned-little-integer).
-define(L,   :32/unsigned-little-integer).
-define(W,   :16/unsigned-little-integer).
-define(B,    :8/unsigned-little-integer).
-define(b,      /bytes).

-include("logon_records.hrl").
-define(N, 16#894B645E89E1535BBDAD5B8B290650530801B18EBFBF5E8FAB3C82872A3E9BB7).
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
        {ok, {A, M, C, N}} ->
            <<U?SH>>  = crypto:sha(<<A?QQ, (Hash#hash.main)?QQ>>),
            S1 = crypto:mod_exp(Hash#hash.second, U, Hash#hash.static),
            S2 = crypto:mod_exp(S1 * A, Hash#hash.r152, Hash#hash.static),
            T0 = binary_to_list(<<S2>>),
            T1 = binary_to_list(crypto:sha(even(T0))),
            T2 = binary_to_list(crypto:sha(odd(T0))),
            SK = merge(T1, T2),

            S   = binary_to_list(crypto:sha(<<(Hash#hash.static)?QQ>>)),
            X   = binary_to_list(crypto:sha(<<7?B>>)),
            SX  = crypto:sha(merge_xor(S, X)),
            AN  = crypto:sha(Account#account.name),
            BSH = binary_to_list(<<SX?b, AN?b, (Hash#hash.r256)?QQ, 
                                   A?QQ, (Hash#hash.main)?QQ>>),
            <<SH?SH>> = crypto:sha(BSH ++ SK),
            ?CHECK,
            io:format("must be equal:~n~p~n~p~n", [M, SH]),
            ?CHECK,
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
    R256     = random:uniform(16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF),
    <<X?SH>> = crypto:sha(<<R256?QQ, (Account#account.hash)?b>>),
    Second   = crypto:mod_exp(7, X, ?N),

    R152     = random:uniform(16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF),
    Temp     = crypto:mod_exp(7, R152, ?N),
    Main     = crypto:mod_exp(Second*3+Temp, 1, ?N),

    R128     = random:uniform(16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF),
    #hash{main=Main, static=?N, second=Second, r152=R152, r256=R256, r128=R128}.

even([X,_,Z]) ->   [X, Z];
even([X,_]) ->     [X];
even([_]) ->       [];
even([X|[_|Z]]) -> [X | even(Z)].

odd([_,X,_]) ->   [X];
odd([_,X]) ->     [X];
odd([X]) ->       [X];
odd([_|[X|Z]]) -> [X | odd(Z)].

merge([], []) ->
    [];
merge([H1|T1], [H2|T2]) ->
    [H1|[H2|merge(T1, T2)]].

merge_xor([], []) ->
    [];
merge_xor([H1|T1], [H2|T2]) ->
    [(H1 bxor H2) | merge_xor(T1, T2)].

close() ->
    io:format("  client socket closed~n", []),
    ok.
