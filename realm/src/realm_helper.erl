-module(realm_helper).
-export([chars/2,
         number_of_chars/2,
         realms/0]).
-import(common_helper, [do/1]).

-include("database_records.hrl").
-include_lib("stdlib/include/qlc.hrl").

%% @spec number_of_chars(int(), int()) -> int().
number_of_chars(AccId, RealmId) ->
    length(chars(AccId, RealmId)).

%% @spec realms() -> list(tuple()).
realms() ->
    do(qlc:q([X || X <- mnesia:table(realm)])).

%% @spec chars(int(), int()) -> list(tuple()).
chars(AccId, RealmId) ->
    do(qlc:q([X || X <- mnesia:table(char),
                   X#char.account_id =:= AccId,
                   X#char.realm_id =:= RealmId])).
