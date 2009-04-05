-module(common_helper).
-export([do/1, game_time/0, ms_time/0, unix_time/0, min/2, max/2]).

do(Q) ->
    F = fun() -> qlc:e(Q) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.

%% @spec game_time() -> int().
game_time() ->
    {Y, Mo, Dm} = erlang:date(),
    {H, Mi, _} = erlang:time(),
    Dw = calendar:day_of_the_week(Y, Mo, Dm),
    GameTime = (((((Mi band 16#3F) bor 
                   (H*64 band 16#7C0)) bor 
                   (Dw*2048 band 16#3800)) bor 
                   ((Dm - 1)*16384 band 16#FC000)) bor 
                   ((Mo - 1)*1048576 band 16#F00000)) bor 
                   ((Y - 2000)*16777216 band 16#1F000000),
    GameTime.


%% @spec ms_time() -> int().
ms_time() ->
    {_, Seconds, Micro} = erlang:now(),
    Seconds * 1000 + Micro div 1000.

%% @spec unix_time() -> int().
unix_time() ->
    {Mega, Seconds, _} = erlang:now(),
    Mega * 1000000 + Seconds.

%% @spec min(int() | float(), int() | float()) -> int() | float().
min(X, Y) when X < Y -> X;
min(_, Y) -> Y.

%% @spec max(int() | float(), int() | float()) -> int() | float().
max(X, Y) when X > Y -> X;
max(_, Y) -> Y.
