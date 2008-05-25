-module(logon_handlers).
-compile(export_all).

-include("logon_records.hrl").

loop(Socket) ->
    case gen_tcp:recv(Socket, 0) of
    {ok, Data} ->
        case logon_packets:match(Data, [auth_request]) of
            {ok, auth_request, Account} ->
                logon_handlers:auth_request(Socket, Account);
            fail ->
                loop(Socket)
        end;
    {error, closed} ->
            ok
    end.

loop(Socket, [AccountRecord]) ->
    case gen_tcp:recv(Socket, 0) of
    {ok, Data} ->
        case logon_packets:match(Data, [auth_request]) of
            {ok, auth_request, Account} ->
                logon_handlers:auth_request(Socket, Account);
            fail ->
                loop(Socket, [AccountRecord])
        end;
    {error, closed} ->
            ok
    end;
loop(Socket, stop) ->
    gen_tcp:close(Socket).

auth_request(Socket, AccountId) ->
    case mnesia:dirty_read({account, AccountId}) of
    [] -> 
        gen_tcp:send(Socket, logon_packets:error(error_account_missing)),
        loop(Socket);
    [AccountRecord] -> 
        gen_tcp:send(Socket, logon_packets:response(auth_request)),
        loop(Socket, [AccountRecord]);
    _ ->
        gen_tcp:send(Socket, logon_packets:error(error_account_missing)),
        loop(Socket)
    end.
