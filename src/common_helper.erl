-module(common_helper).
-export([do/1, now/0, game_time/1]).

do(Q) ->
    F = fun() -> qlc:e(Q) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.

now() ->
    {Y, Mo, Dm} = erlang:date(),
    {H, Mi, S} = erlang:time(),
    {Y, Mo, Dm, H, Mi, S}.

game_time({Y, Mo, Dm, H, Mi, S}) ->
    Dw = calendar:day_of_the_week(Y, Mo, Dm),
    GameTime = (((((Mi band 16#3F) bor 
                   (H*64 band 16#7C0)) bor 
                   (Dw*2048 band 16#3800)) bor 
                   ((Dm - 1)*16384 band 16#FC000)) bor 
                   ((Mo - 1)*1048576 band 16#F00000)) bor 
                   ((Y - 2000)*16777216 band 16#1F000000),
    GameTime.