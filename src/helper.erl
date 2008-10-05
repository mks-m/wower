-module(helper).
-export([start/0, start/1, stop/0, restart/1, install/0, compile/0, load/0]).

-include("logon_records.hrl").
-include("realm_records.hrl").
-include("database_records.hrl").

start() ->
    logon_server:start(),
    realm_server:start(),
    ok.

start(Method) ->
    ok = ?MODULE:Method(),
    ok = ?MODULE:start(),
    ok.

stop() ->
    logon_server:stop(),
    realm_server:stop(),
    ok.

restart(Method) ->
    logon_server:restart(Method),
    realm_server:restart(Method),
    ok.

compile() ->
    logon_server:compile(),
    realm_server:compile(),
    ok.

load() ->
    logon_server:load(),
    realm_server:load(),
    ok.

install() ->
    mnesia:delete_schema([node()]),
    mnesia:create_schema([node()]),
    mnesia:start(),
    create_accounts(),
    create_realms(),
    create_chars(),
    mnesia:stop(),
    ok.

create_accounts() ->
    mnesia:create_table(account, [{attributes, record_info(fields, account)}, 
                                  {disc_copies, [node()]}]),
    mnesia:dirty_write(account, #account{id = 1,
                                         name     = "TEST", 
                                         password = "TEST"}).

create_realms() ->
    mnesia:create_table(realm, [{attributes, record_info(fields, realm)},
                                {disc_copies, [node()]}]),
    mnesia:dirty_write(realm, #realm{id = 1,
                                     name       = "Test Realm (127.0.0.1)", 
                                     icon       = 0, 
                                     lock       = 0, 
                                     status     = 0, 
                                     address    = "127.0.0.1:8640", 
                                     population = 1.0, 
                                     timezone   = 2}).

create_chars() ->
    mnesia:create_table(char, [{attributes, record_info(fields, char)},
                                {disc_copies, [node()]}]),
    mnesia:dirty_write(char, #char{id             = 1, 
                                   account_id     = 1, 
                                   realm_id       = 1, 
                                   name           = "Moo", 
                                   player_bytes   = <<(char_helper:race(dwarf)):8,
                                                      (char_helper:class(warrior)):8,
                                                      (char_helper:gender(male)):8,
                                                      1:8,
                                                      1:8,
                                                      1:8,
                                                      1:8,
                                                      1:8>>,
                                   level          = 1,
                                   position_x     = 29.9, 
                                   position_y     = 71.3, 
                                   position_z     = 50.0, 
                                   map_id         = 0,
                                   zone_id        = 1,
                                   guild_id       = 0, 
                                   general_flags  = 64, 
                                   at_login_flags = 0}).
