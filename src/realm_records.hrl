-record(client_state, {account, realm, sender, receiver, latency, 
                       char, dbc_chr_race, fields, current_map}).
-record(crypt_state, {i, j, key}).