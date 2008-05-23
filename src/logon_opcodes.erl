%% Author: kp
%% Created: 24 Mar 2008
%% Description: TODO: Add description to logon_opcodes
-module(logon_opcodes).

-compile([export_all]).

c_register_realm()				-> 1.
s_realm_registered()			-> 2.
c_request_session() 			-> 3.
s_session_result() 				-> 4.
c_ping()						-> 5.
s_pong()						-> 6.
c_sql_execute() 				-> 7.
c_reload_accounts() 			-> 8.
c_auth_request() 				-> 9.
s_auth_reply() 					-> 10.
s_character_mapping_request() 	-> 11.
c_character_mapping_reply() 	-> 12.
c_character_mapping_update() 	-> 13.
s_disconnect() 					-> 14.
c_console_login_request() 		-> 15.
s_console_login_reply() 		-> 16.
c_modify_database() 			-> 17.
c_count() 						-> 18.

opcode( 1) -> c_register_realm;
opcode( 2) -> s_realm_registered;
opcode( 3) -> c_request_session;
opcode( 4) -> s_session_result;
opcode( 5) -> c_ping;
opcode( 6) -> s_pong;
opcode( 7) -> c_sql_execute;
opcode( 8) -> c_reload_accounts;
opcode( 9) -> c_auth_request;
opcode(10) -> s_auth_reply;
opcode(11) -> s_character_mapping_request;
opcode(12) -> c_character_mapping_reply;
opcode(13) -> c_character_mapping_update;
opcode(14) -> s_disconnect;
opcode(15) -> c_console_login_request;
opcode(16) -> s_console_login_reply;
opcode(17) -> c_modify_database;
opcode(18) -> c_count.