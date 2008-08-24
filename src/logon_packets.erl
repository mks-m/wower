-module(logon_packets).
-export([receiver/2, encoder/1]).

-define(IN, /unsigned-little-integer).
-define(QQ, :256?IN).
-define(SH, :160?IN).
-define(DQ, :128?IN).
-define(Q,   :64?IN).
-define(L,   :32?IN).
-define(W,   :16?IN).
-define(B,    :8?IN).
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
    G        = 7,
    Salt     = 16#B4A2BA1BECF0034B869FA1BE8460C73C69C84FAF43710A1F0700D7E68F4531EB, % random:uniform(16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF),
    B        = 16#73CA17CFC1E4EA1A30A169D3BF471C0962622785818C4FCE903128D82258BE25, % random:uniform(16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF),
    <<X?SH>> = crypto:sha(<<Salt?QQ, (Account#account.hash)?b>>),
    Verifier = crypto:mod_exp(G, X, ?N),
    Temp     = crypto:mod_exp(G, B, ?N),
    Public   = crypto:mod_exp(Verifier*3+Temp, 1, ?N),
    #hash{public=Public, secret=B, modulus=?N, verifier=Verifier, salt=Salt}.

proof(A, M, H, P) ->
    <<U?SH>>  = crypto:sha(<<A?QQ, (H#hash.public)?QQ>>),
    S1 = crypto:mod_exp(H#hash.verifier, U, ?N),
    S2 = crypto:mod_exp(S1 * A, H#hash.secret, ?N),
    T0 = binary_to_list(<<S2:320?IN>>),
    T1 = binary_to_list(crypto:sha(even(T0))),
    T2 = binary_to_list(crypto:sha(odd(T0))),
    SK = merge(T1, T2),
    <<S?SH>> = crypto:sha(<<(?N)?QQ>>),
    <<X?SH>> = crypto:sha(<<7?B>>),
    SX  = crypto:sha(<<(S bxor X)?SH>>),
    AN  = crypto:sha(P#account.name),
    BSH = binary_to_list(<<SX?b, AN?b, (H#hash.salt)?QQ, 
                           A?QQ, (H#hash.public)?QQ>>),
    SH  = crypto:sha(BSH ++ SK),
    io:format("must be equal:~n~p~n~p~n", [<<M?SH>>, SH]),
    ok.

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
    [H1,H2|merge(T1, T2)].

close() ->
    io:format("  client socket closed~n", []),
    ok.
