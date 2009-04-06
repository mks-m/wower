% network defines
-define(I, /unsigned-little-integer).
-define(O, /unsigned-big-integer).
-define(b,  /bytes).                   % that's for plain text
-define(f,  :32/float-little).         % float values obviously
-define(SH, :160?I).                   % SH is for sha1
-define(Q,   :64?I).                   % uint64 - Q for quad
-define(L,   :32?I).                   % uint32 - L for long
-define(W,   :16?I).                   % uint16 - W for word
-define(B,    :8).                     % byte (doesn't need to be endianated)

% size, location and object records
-record(vector, {x, y, z}).

-define(DEBUG, true).

-ifdef(DEBUG).
-define(DINFO(S), io:format(S)).
-define(DDINFO(S, P), io:format(S, P) end).
-define(DEXEC(V), V).
-else.
-define(DINFO(S), ok).
-define(DDINFO(S, P), ok).
-define(DEXEC(V), ok).
-endif.
