-module(srp6).
-export([challenge/1, proof/2, test/0]).

-define(IN, /unsigned-little-integer).
-define(NI, /unsigned-big-integer).
-define(QQ, :256?IN).
-define(SH, :160?IN).
-define(DQ, :128?IN).
-define(Q,   :64?IN).
-define(L,   :32?IN).
-define(W,   :16?IN).
-define(B,    :8?IN).
-define(b,      /bytes).

challenge(Credentials) ->
    Modulus   = 16#894B645E89E1535BBDAD5B8B290650530801B18EBFBF5E8FAB3C82872A3E9BB7, 
    Generator = 7,
    Salt      = 16#B4A2BA1BECF0034B869FA1BE8460C73C69C84FAF43710A1F0700D7E68F4531EB, % will be random
    SecretB   = 16#73CA17CFC1E4EA1A30A169D3BF471C0962622785818C4FCE903128D82258BE25, % will be random
    X         = sha(<<Salt:256?NI, Credentials:160?NI>>),
    io:format("X = ~.16B~n", [X]),
    Verifier  = crypto:mod_exp(Generator, X, Modulus),
    PublicB   = crypto:mod_exp(Verifier * 3 + crypto:mod_exp(Generator, SecretB, Modulus), 1, Modulus),
    {Modulus, Generator, PublicB, SecretB, Verifier, Salt}.

proof(PublicA, MProof1) ->
    ok.

sha(Data) ->
    <<Result:160/big>> = crypto:sha(Data),
    Result.

test() ->
    User = "WCELL",
    Pass = "RULES",
    Credentials = list_to_binary(User ++ ":" ++ Pass),
    Hash        = sha(Credentials),
    io:format("C: ~.16B~n", [Hash]),
    {N, G, PB, SB, V, S} = challenge(Hash).
