-module(account_helper).
-export([find_by_name/1,
         find_or_create_by_name/1,
         get_whois/1]).
-import(common_helper, [do/1]).

-include("database_records.hrl").
-include_lib("stdlib/include/qlc.hrl").

%% @spec find_by_name(list()) -> {ok, tuple()} | {error, not_found}.
find_by_name(Name) ->
    Result = do(qlc:q([X || X <- mnesia:table(account),
                            X#account.name =:= Name])),
    case Result of
    [Account] -> {ok, Account};
    _ -> {error, not_found}
    end.

find_or_create_by_name(Name) ->
    case find_by_name(Name) of
    {error, not_found} ->
        io:format("Account ~p not found, attempting to create~n", [Name]),
        Create = fun() ->
            Q = qlc:q([X#account.id || X <- mnesia:table(account)]),
            {atomic, Ids} = mnesia:transaction(fun() -> qlc:e(Q) end),
            Id = lists:max(Ids) + 1,
            A = #account{id = Id, name = Name, password = Name},
            mnesia:write(A)
        end,
        {atomic, _} = mnesia:transaction(Create),
        find_by_name(Name);
    Result -> Result
    end.

%% @spec get_whois(any()) -> list().
get_whois(_) ->
    "Moo's account is TEST, e-mail: test@test.com, last ip: 192.168.0.100".
