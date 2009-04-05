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

-define(DEBUG, false).
-define(DINFO(S), if ?DEBUG -> io:format(S); true -> ok end).
-define(DDINFO(S, P), if ?DEBUG -> io:format(S, P); true -> ok end).
-define(DEXEC(V), if ?DEBUG -> V; true -> ok end).
