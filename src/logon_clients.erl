-module(logon_clients).
-compile(export_all).

-record(state, {account=null, hash=null}).

new() ->
    initial(#state{}).

initial(State) ->
    receive
        {auth_request, Account} ->
            Account;
        _ ->
            initial(State)
    after 
        50 -> initial(State)
    end.
