-record(account, {name, password="", banned=false}).
-record(hash, {modulus=16#B79B3E2A87823CAB8F5EBFBF8EB10108535006298B5BADBD5B53E1895E644B89, 
               generator=7, public, secret, verifier, salt}).
