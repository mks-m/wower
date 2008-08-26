-module(srp6).
-export([challenge/1, proof/3, sha/1, test/0]).
-include("logon_records.hrl").

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

challenge(A) ->
    Credentials      = sha(A#account.name ++ ":" ++ A#account.password),
    H = #hash{salt   = 16#33f140d46cb66e631fdbbbc9f029ad8898e05ee533876118185e56dde843674f,
              secret = 16#8692E3A6BA48B5B1004CEF76825127B7EB7D1AEF},
    X = sha(<<(H#hash.salt)?QQ?NI, Credentials?SH?NI>>),
    Verifier  = crypto:mod_exp(H#hash.generator, X, H#hash.modulus),
    Temp      = crypto:mod_exp(H#hash.generator, H#hash.secret, H#hash.modulus),
    PublicB   = crypto:mod_exp(Verifier * 3 + Temp, 1, H#hash.modulus),
    H#hash{public=PublicB, verifier=Verifier}.

proof(A, H, P) ->
    U = sha(<<A?QQ?NI, (H#hash.public)?QQ?NI>>),
    S1 = crypto:mod_exp(H#hash.verifier, U, H#hash.modulus),
    S2 = crypto:mod_exp(S1 * A, H#hash.secret, H#hash.modulus),
    T0  = binary_to_list(<<S2?QQ?IN>>),
    T1  = binary_to_list(<<(sha(even(T0)))?SH?NI>>),
    T2  = binary_to_list(<<(sha(odd(T0)))?SH?NI>>),
    SK  = merge(T1, T2),
    io:format("S: ~p~n", [SK]), 
    S   = sha(<<(H#hash.modulus)?QQ?NI>>),
    X   = sha(<<(H#hash.generator)?B>>),
    SX  = S bxor X,
    AN  = sha(P#account.name),
    BSH = binary_to_list(<<SX?SH?NI, AN?SH?NI, (H#hash.salt)?QQ?NI, 
                           A?QQ?NI, (H#hash.public)?QQ?NI>>),
    sha(BSH ++ SK).

test() ->
    A = 16#232fb1b88529643d95b8dce78f2750c75b2df37acba873eb31073839eda0738d,
    P = #account{name = "TEST", password = "TEST"},
    H = challenge(P),
    M = proof(A, H, P),
    io:format("~.16B~n", [M]).

sha(Data) ->
    <<Result:160?NI>> = crypto:sha(Data),
    Result.

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
