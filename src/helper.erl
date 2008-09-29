-module(helper).
-export([start/0, start/1, stop/0, restart/1, install/0, compile/0, load/0]).

-include("logon_records.hrl").
-include("realm_records.hrl").
-include("database_records.hrl").

start() ->
    logon_server:start(),
    realm_server:start().

start(Method) when Method =:= compile orelse Method =:= load ->
    ?MODULE:Method().

stop() ->
    logon_server:stop(),
    realm_server:stop().

restart(Method) ->
    logon_server:restart(Method),
    realm_server:restart(Method).

compile() ->
    logon_server:compile(),
    realm_server:compile().

load() ->
    logon_server:load(),
    realm_server:load().

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
                                   race           = race_dwarf, 
                                   class          = class_warrior, 
                                   gender         = gender_male,
                                   position_x     = 0.0, 
                                   position_y     = 0.0, 
                                   position_z     = 0.0, 
                                   guild_id       = 0, 
                                   general_flags  = 0, 
                                   at_login_flags = 0}).
