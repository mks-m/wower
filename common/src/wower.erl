-module(wower).
-author('keymone <keymone@gmail.com>').
-behaviour(application).
-export([start/0, stop/0]).
-export([start/2, stop/1]).

start() ->
    application:start(crypto),
    application:start(mnesia),
    application:start(wower).

stop() ->
    application:stop(wower).

start(_Type, _StartArgs) ->
    mnesia:wait_for_tables([account, realm, character], 1000),
    cell:start(),
    ets:new(connected_clients, [named_table, set, public]),
    wower_sup:start_link().

stop(_State) ->
    cell:stop(),
    gen_server:call(realm_server, stop),
    gen_server:call(logon_server, stop).
