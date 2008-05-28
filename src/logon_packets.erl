-module(logon_packets).
-export([receiver/2, encoder/1]).

-include("logon_records.hrl").

error(C) when atom(C) ->
    <<0:8, 0:8, (logon_opcodes:get(C)):8>>;
error(C) when integer(C) ->
    <<0:8, 0:8, C:8>>;
error(C) ->
    io:format("wrong errorcode: ~p~n", [C]),
    <<0:8, 0:8, 1:8>>.

%%
%% receiver can receive only auth_request packet
%% and will try to find account and generate 
%% authentication hash for connection
%% will switch to decoder if such hash generated
%%
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

decoder(Socket, Pid, Hash) ->
    case gen_tcp:recv(Socket, 0) of
    {ok, _Data} ->
        %% decode packet here
        %% match packet structure
        %% call handler
        decoder(Socket, Pid, Hash);
    {error, closed} ->
        ok
    end.

encoder(Hash) ->
    Hash.

hash(Account) ->
    S = random:uniform(16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF),
    B = random:uniform(16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF),
    N = 16#894B645E89E1535BBDAD5B8B290650530801B18EBFBF5E8FAB3C82872A3E9BB7,
    <<H:256/integer>> = crypto:sha(<<S:256, (Account#account.hash)/binary>>),
    X = crypto:mod_exp(7, H, N),
    ok.

rpc(Pid, Data) ->
    Pid ! {self(), Data},
    receive
    {Pid, Response} -> 
        {self(), Response}
    end.