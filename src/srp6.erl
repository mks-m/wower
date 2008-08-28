-module(srp6).
-export([challenge/1, proof/3]).
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
    Credentials      = crypto:sha(A#account.name ++ ":" ++ A#account.password),
    H = #hash{salt   = random:uniform(16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF),
              secret = random:uniform(16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF)},
    X                = sha(<<(H#hash.salt)?QQ?IN, Credentials?b>>),
    Verifier         = crypto:mod_exp(H#hash.generator, X, H#hash.modulus),
    Temp             = crypto:mod_exp(H#hash.generator, H#hash.secret, H#hash.modulus),
    PublicB          = crypto:mod_exp(Verifier * 3 + Temp, 1, H#hash.modulus),
    H#hash{public=PublicB, verifier=Verifier}.

proof(A, H, P) ->
    U   = sha(<<A?QQ?IN, (H#hash.public)?QQ?IN>>),
    S1  = crypto:mod_exp(H#hash.verifier, U, H#hash.modulus),
    S2  = crypto:mod_exp(S1 * A, H#hash.secret, H#hash.modulus),
    T0  = binary_to_list(<<S2?QQ?IN>>),
    T1  = binary_to_list(<<(sha(even(T0)))?SH?NI>>),
    T2  = binary_to_list(<<(sha(odd(T0)))?SH?NI>>),
    SK  = lists:reverse(merge(T1, T2)),
    S   = sha(<<(H#hash.modulus)?QQ?IN>>),
    X   = sha(<<(H#hash.generator)?B>>),
    SX  = S bxor X,
    AN  = sha(P#account.name),
    CP  = sha(binary_to_list(<<SX?SH?IN, AN?SH?IN, (H#hash.salt)?QQ?IN, 
                               A?QQ?IN, (H#hash.public)?QQ?IN>>) ++ SK),
    SP  = sha(binary_to_list(<<A?QQ?IN, CP?SH?IN>>) ++ SK),
    H#hash{session_key = SK, client_proof = CP, session_proof = SP}.

sha(Data) ->
    <<Result:160?IN>> = crypto:sha(Data),
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
    [H2,H1|merge(T1, T2)].
