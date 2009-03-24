-module(handlers_misc).
-compile(export_all).

-include("realm_records.hrl").
-include("common.hrl").

cmsg_ping(S, State, Data) ->
    {Sequence, Latency} = realm_patterns:cmsg_ping(Data),
    S ! {self(), smsg_pong, <<Sequence?L>>},
    State#client_state{latency=Latency}.
	
cmsg_logout_request(S, State, _Data) ->
	S ! {self(), smsg_logout_response, <<0?L, 0?B>>},
	C = self(),
	LogPid = spawn(fun() ->
		receive
			{C, exit} -> ok
		after 20000 ->
			C ! logout,
			S ! {C, smsg_logout_complete, <<>>}
		end
		end),
	State#client_state{logout=LogPid}.

cmsg_logout_cancel(S, State, _Data) when pid(State#client_state.logout) ->
	Logout = State#client_state.logout,
	Logout ! {self(), exit},
	S ! {self(), smsg_logout_cancel_ack, <<>>},
	State#client_state{logout = no};
	
cmsg_logout_cancel(_S, State, _Data) ->
	State.