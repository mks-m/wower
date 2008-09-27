-record(account, {id, name, password=""}).
-record(realm,   {id, name, icon, lock, status, address, population, timezone}).
-record(char,    {id, accout_id, realm_id, name, race, class, gender, 
                  position_x, position_y, position_z, guild_id, general_flags, 
                  at_login_flags}).