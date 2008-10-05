-record(account, {id, name, password=""}).
-record(realm,   {id, name, icon, lock, status, address, population, timezone}).
-record(char,    {id, account_id, realm_id, name, player_bytes, level, 
                  position_x, position_y, position_z, map_id, zone_id, 
                  guild_id, general_flags, at_login_flags}).