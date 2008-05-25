-module(logon_server).

-export([start/0, listen/1, accept/2, install/0]).

-define(PORT, 3724).
-define(OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

-include("logon_records.hrl").

start() ->
    mnesia:start(),
    mnesia:wait_for_tables([accounts], 20000),
    spawn(?MODULE, listen, [[]]).

install() ->
    mnesia:delete_schema([node()]),
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(account, [{attributes, record_info(fields, account)},
                                  {disc_copies, [node()]}]),
    mnesia:stop(),
    ok.

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
            spawn(logon_handlers, loop, [Socket]),
            accept(LSocket, [Socket | Clients]);
        {error, closed} ->
            [ gen_tcp:close(Socket) || Socket <- Clients ],
            gen_tcp:close(LSocket),
            ok
    end.
