-module(logon_server).

-export([start/0, listen/1, handle/1, accept/2, install/0]).

-define(PORT, 3724).
-define(OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

-define(L, 32/unsigned-little-integer).
-define(W, 16/unsigned-little-integer).
-define(B,  8/unsigned-little-integer).
-define(b,   /bytes).

-define(ERR_SUCCESS,             0).
-define(ERR_IPBAN,               1).
-define(ERR_ACCOUNT_CLOSED,      3).
-define(ERR_NO_ACCOUNT,          4).
-define(ERR_ACCOUNT_IN_USE,      6).
-define(ERR_PREORDER_TIME_LIMIT, 7).
-define(ERR_SERVER_FULL,         8).
-define(ERR_WRONG_BUILD_NUMBER,  9).
-define(ERR_UPDATE_CLIENT,      10).
-define(ERR_ACCOUNT_FREEZED,    12).

-define(AUTH_REQUEST,  <<Cmd:?B, Err:?B, Size:?W, Game:4?b, 
                         Major:?B, Middle:?B, Minor:?B, Build:?W, 
                         Platform:4?b, Os:4?b, Country:4?b,
                         TimeZone:?L, IP:4?b, Length:?B, Account?b>>).
-define(AUTH_RESPONSE, <<>>).
-define(AUTH_ERROR,    <<0:8, 0:8, Error:8>>).

-record(account, {name, password, banned}).

start() ->
    mnesia:start(),
    mnesia:wait_for_tables([accounts], 20000),
    spawn(?MODULE, listen, [[]]).

listen([]) ->
    {ok, LSocket} = gen_tcp:listen(?PORT, ?OPTIONS),
    spawn(?MODULE, accept, [LSocket, []]),
    loop_listener(LSocket).

loop_listener(LSocket) ->
    receive
        stop ->
            gen_tcp:close(LSocket);
        _ ->
            loop_listener(LSocket)
    end.

accept(LSocket, Clients) ->
    case gen_tcp:accept(LSocket) of
        {ok, Socket} ->
            spawn(?MODULE, handle, [Socket]),
            accept(LSocket, [Socket | Clients]);
        {error, closed} ->
            [ gen_tcp:close(Socket) || Socket <- Clients ],
            gen_tcp:close(LSocket),
            ok
    end.

handle(Socket) ->
    handle(Socket, []).

handle(Socket, []) ->
    case gen_tcp:recv(Socket, 0) of
    {ok, ?AUTH_REQUEST} ->
        case handle_auth_request(Account) of
        {ok, AccountRecord, Response} ->
            gen_tcp:send(Socket, Response),
            handle(Socket, [AccountRecord]);
        {fail, Response} ->
            gen_tcp:send(Socket, Response),
            handle(Socket, []);
        {error, Reason} ->
            io:format("error in handle auth request: ~p~n", [Reason])
        end;
    {error, closed} ->
            ok
    end;
handle(Socket, Args) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
             io:format("unknown data ~p~n", [Data]),
            gen_tcp:send(Socket, Data),
            handle(Socket, Args);
        {error, closed} ->
            ok
    end.

handle_auth_request(AccountId) ->
    case mnesia:read({account, AccountId}) of
    [AccountRecord] -> {ok, AccountRecord, ?AUTH_RESPONSE};
    [] -> Error = ?ERR_NO_ACCOUNT, {fail, ?AUTH_ERROR};
    _ -> Error = ?ERR_IPBAN, {fail, ?AUTH_ERROR}
    end.

install() ->
    mnesia:delete_schema([node()]),
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(account, [{attributes, record_info(fields, account)},
                                  {disc_copies, [node()]}]),
    mnesia:stop(),
    ok.
