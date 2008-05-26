-module(logon_packets).
-export([receiver/2]).

error(C) when atom(C) ->
    <<0:8, 0:8, (logon_opcodes:get(C)):8>>;
error(C) when integer(C) ->
    <<0:8, 0:8, C:8>>;
error(C) ->
    io:format("wrong errorcode: ~p~n", [C]),
    <<0:8, 0:8, 1:8>>.

receiver(Socket, Pid) ->
    case gen_tcp:recv(Socket, 0) of
    {ok, Data} ->
        case logon_patterns:auth_request(Data) of
        {ok, auth_request, Account} ->
            case mnesia:dirty_read({account, Account}) of
            [] -> 
                gen_tcp:send(Socket, error(error_account_missing)),
                receiver(Socket, Pid);
            [AccountRecord] -> 
                Response = rpc(Pid, {auth_request, AccountRecord}),
                gen_tcp:send(Socket, logon_patterns:encode(Response)),
                decoder(Socket, Pid, hash(AccountRecord));
            _ ->
                gen_tcp:send(Socket, error(error_account_missing)),
                receiver(Socket, Pid)
            end;
        fail ->
            receiver(Socket, Pid)
        end;
    {error, closed} ->
        ok
    end.

hash(_) ->
    ok.

decoder(Socket, Pid, Hash) ->
    case gen_tcp:recv(Socket, 0) of
    {ok, Data} ->
        case logon_patterns:auth_request(Data) of
        {ok, auth_request, Account} ->
            Hash = hash(Account),
            decoder(Socket, Pid, Hash);
        fail ->
            receiver(Socket, Pid)
        end;
    {error, closed} ->
        ok
    end.

rpc(Pid, Data) ->
    Pid ! Data,
    response(Pid).

response(Pid) ->
    receive
    {Pid, Response} -> 
        Response;
    _ -> 
        response(Pid)
    end.