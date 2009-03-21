-record(hash,        {modulus=16#894B645E89E1535BBDAD5B8B290650530801B18EBFBF5E8FAB3C82872A3E9BB7, 
                      generator=7, public, secret, verifier, salt, session_key, session_proof, client_proof}).
-record(logon_state, {authenticated=no, hash=null, account=null, receiver, sender}).
