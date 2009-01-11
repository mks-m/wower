-module(helper).
-export([main/1, start/0, start/1, stop/0, restart/1, install/0, compile/0, load/0]).

-include("logon_records.hrl").
-include("realm_records.hrl").
-include("database_records.hrl").
-include("more_records.hrl").

main(["install"]) ->
    install(),
    start();
main(_) ->
    start().

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
    c:c(logon_server),
    c:c(realm_server),
    c:c(dbc),
    logon_server:compile(),
    realm_server:compile(),
    ok.

load() ->
    c:l(logon_server),
    c:l(realm_server),
    c:l(dbc),
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
    dbc:import_all(),
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
    CreateInfo  = content:char_create_info(dwarf, warrior),
    Char = #char{id               = random:uniform(16#FFFFFFFF),
                 account_id       = 1,
                 realm_id         = 1,
                 name             = "Moo",
                 race             = dwarf, 
                 gender           = male, 
                 class            = warrior,
                 skin             = 8, 
                 face             = 9, 
                 hair_style       = 4, 
                 hair_color       = 0,
                 facial_hair      = 1,
                 level            = 1,
                 guild_id         = 0,
                 general_flags    = 16#10A00040,
                 at_login_flags   = 0,
                 faction_template = CreateInfo#char_create_info.faction_template, 
                 map_id           = CreateInfo#char_create_info.map_id, 
                 zone_id          = CreateInfo#char_create_info.zone_id, 
                 position_x       = CreateInfo#char_create_info.position_x, 
                 position_y       = CreateInfo#char_create_info.position_y, 
                 position_z       = CreateInfo#char_create_info.position_z, 
                 orientation      = CreateInfo#char_create_info.orientation, 
                 display_id       = CreateInfo#char_create_info.display_id, 
                 strength         = CreateInfo#char_create_info.strength, 
                 agility          = CreateInfo#char_create_info.agility,
                 stamina          = CreateInfo#char_create_info.stamina, 
                 intellect        = CreateInfo#char_create_info.intellect, 
                 spirit           = CreateInfo#char_create_info.spirit, 
                 health           = CreateInfo#char_create_info.health, 
                 mana             = CreateInfo#char_create_info.mana, 
                 focus            = CreateInfo#char_create_info.focus, 
                 power            = CreateInfo#char_create_info.power, 
                 power_type       = CreateInfo#char_create_info.power_type, 
                 intro            = CreateInfo#char_create_info.intro,
                 attack_power     = CreateInfo#char_create_info.attack_power, 
                 min_dmg          = CreateInfo#char_create_info.min_dmg, 
                 max_dmg          = CreateInfo#char_create_info.max_dmg, 
                 scale            = CreateInfo#char_create_info.scale},
    
    mnesia:dirty_write(char, Char).
