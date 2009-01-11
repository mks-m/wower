-record(client_state, {account, realm, sender, receiver, latency, 
                       char, dbc_chr_race, fields, current_map}).
-record(crypt_state, {i, j, key}).
-record(movement_info, {flags, unk1, time, x, y, z, o, 
                        t_guid, tx, ty, tz, to, t_time, t_seat, 
                        s_pitch, fall_time, unk2, 
                        j_sin, j_cos, j_speed, unk3}).
