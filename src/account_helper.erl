-module(account_helper).
-export([find_by_name/1,
         get_whois/1]).
-import(common_helper, [do/1]).

-include("database_records.hrl").
-include_lib("stdlib/include/qlc.hrl").

find_by_name(Account) ->
    do(qlc:q([X || X <- mnesia:table(account), X#account.name =:= Account])).

get_whois(_) ->
    "Moo's account is TEST, e-mail: test@test.com, last ip: 192.168.0.100".
