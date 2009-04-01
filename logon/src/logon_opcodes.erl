-module(logon_opcodes).
-compile(export_all).

%% @spec error(atom()) -> int().
error(success)         ->  0;
error(ipban)           ->  1;
error(account_closed)  ->  3;
error(account_missing) ->  4;
error(account_in_use)  ->  6;
error(preorder_limit)  ->  7;
error(server_full)     ->  8;
error(wrong_build)     ->  9;
error(update_client)   -> 10;
error(account_freezed) -> 12.

%% @spec get(int()) -> atom().
get( 0) -> authenticate;
get( 1) -> proof;
get(16) -> realmlist;
get(__) -> wrong_opcode.
