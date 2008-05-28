-module(logon_server).

-export([start/0, listen/0, accept/2, install/0]).

-define(PORT, 3724).
-define(OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

-include("logon_records.hrl").

start() ->
    crypto:start(),
    mnesia:start(),
    mnesia:wait_for_tables([account], 20000),
    spawn(?MODULE, listen, []).

install() ->
    mnesia:delete_schema([node()]),
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(account, [{attributes, record_info(fields, account)},
                                  {disc_copies, [node()]}]),
    mnesia:dirty_write(account, #account{name="TEST", 
                                         password="TSET", 
                                         hash=crypto:sha("test:tset")}),
    mnesia:stop(),
    ok.

listen() ->
    {ok, LSocket} = gen_tcp:listen(?PORT, ?OPTIONS),
    spawn(?MODULE, accept, [LSocket, []]),
    receive stop -> gen_tcp:close(LSocket) end.

accept(LSocket, Clients) ->
    case gen_tcp:accept(LSocket) of
        {ok, Socket} ->
            Client = spawn(logon_clients, new, []),
            spawn(logon_packets, receiver, [Socket, Client]),
            accept(LSocket, [Socket | Clients]);
        {error, closed} ->
            [ gen_tcp:close(Socket) || Socket <- Clients ],
            gen_tcp:close(LSocket),
            ok
    end.
