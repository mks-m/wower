-module(realm_helper).
-export([number_of_chars/2,
         realms/0]).
-import(common_helper, [do/1]).

-include("database_records.hrl").
-include_lib("stdlib/include/qlc.hrl").

number_of_chars(AccId, RealmId) ->
    length(do(qlc:q([X || X <- mnesia:table(char),
                          X#char.account_id =:= AccId,
                          X#char.realm_id =:= RealmId]))).

realms() ->
    do(qlc:q([X || X <- mnesia:table(realm)])).