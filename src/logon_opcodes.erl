-module(logon_opcodes).
-compile(export_all).

get(error_success) -> 0;
get(0) -> error_success;

get(error_ipban) -> 1;
get(1) -> error_ipban;

get(error_account_closed) -> 3;
get(3) -> error_account_closed;

get(error_account_missing) -> 4;
get(4) -> error_account_missing;

get(error_account_in_use) -> 6;
get(6) -> error_account_in_use;

get(error_preorder_limit) -> 7;
get(7) -> error_preorder_limit;

get(error_server_full) -> 8;
get(8) -> error_server_full;

get(error_wrong_build) -> 9;
get(9) -> error_wrong_build;

get(error_update_client) -> 10;
get(10) -> error_update_client;

get(error_account_freezed) -> 12;
get(12) -> error_account_freezed.
