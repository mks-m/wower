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
    H = #hash{salt   = 16#B4A2BA1BECF0034B869FA1BE8460C73C69C84FAF43710A1F0700D7E68F4531EB,
              secret = 16#73CA17CFC1E4EA1A30A169D3BF471C0962622785818C4FCE903128D82258BE25},
    X = sha(<<(H#hash.salt)?QQ?NI, Credentials?SH?NI>>),
    h("X: ", X),
    Verifier  = crypto:mod_exp(H#hash.generator, X, H#hash.modulus),
    h("V: ", Verifier),
    Temp      = crypto:mod_exp(H#hash.generator, H#hash.secret, H#hash.modulus),
    PublicB   = crypto:mod_exp(Verifier * 3 + Temp, 1, H#hash.modulus),
    h("B: ", PublicB),
    H#hash{public=PublicB, verifier=Verifier}.

proof(A, H, P) ->
    U = sha(<<A?QQ?NI, (H#hash.public)?QQ?NI>>),
    h("U: ", U),
    S1 = crypto:mod_exp(H#hash.verifier, U, H#hash.modulus),
    S2 = crypto:mod_exp(S1 * A, H#hash.secret, H#hash.modulus),
    h("S: ", S2),
    T0  = binary_to_list(<<S2?QQ?IN>>),
    T1  = binary_to_list(<<(sha(even(T0)))?SH?NI>>),
    T2  = binary_to_list(<<(sha(odd(T0)))?SH?NI>>),
    SK  = merge(T1, T2),
    d("K: ", SK),
    S   = sha(<<(H#hash.modulus)?QQ?NI>>),
    X   = sha(<<(H#hash.generator)?B>>),
    SX  = S bxor X,
    AN  = sha(P#account.name),
    h("N: ", AN),
    BSH = binary_to_list(<<SX?SH?NI, AN?SH?NI, (H#hash.salt)?QQ?NI, 
                           A?QQ?NI, (H#hash.public)?QQ?NI>>),
    sha(BSH ++ SK).

test() ->
    A = 16#1f3c28d19eb8ee80c48792d350798f69b3135b358f5a31e9fcc6494ec4229561,
    P = #account{name = "WCELL", password = "RULES"},
    H = challenge(P),
    M = proof(A, H, P),
    h("M: ", M).

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

h(M, X) ->
    io:format("~n~p~.16B~n", [M, X]).

d(M, X) ->
    io:format("~n~p~p~n", [M, X]).