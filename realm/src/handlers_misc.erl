-module(handlers_misc).
-compile(export_all).

-include("realm_records.hrl").
-include("common.hrl").

cmsg_ping(S, State, Data) ->
    {Sequence, Latency} = realm_patterns:cmsg_ping(Data),
    S ! {self(), smsg_pong, <<Sequence?L>>},
    State#client_state{latency=Latency}.
	
