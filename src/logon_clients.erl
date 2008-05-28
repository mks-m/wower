-module(logon_clients).
-compile(export_all).

-record(state, {account=null, hash=null}).

new() ->
    initial(#state{}).

initial(#state{account=null, hash=null}) ->
    receive
        {authenticate, Account} -> 
            initial(#state{account=Account})
    end.
