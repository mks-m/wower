-module(test_performance_cell).
-author('keymone <keymone@gmail.com').

prepare(N) ->
    C = cell:create(),
    add(N, C).

add(0, C) ->
    C;
add(N, C) ->
    X = random:uniform(65000.0) - 32500.0,
    Y = random:uniform(65000.0) - 32500.0,
    Z = random:uniform(65000.0) - 32500.0,
    cell:add(spawn(?MODULE, tester, [C, {X, Y, Z}]), {X, Y, Z}),
    C ! {add, spawn(?MODULE, tester, [C, V]), V#vector.x, V#vector.y, V#vector.z},
    test(N-1, C).

%test(0, C) ->
%    C;
%test(N, C) ->
%    V = #vector{x = random:uniform(65000.0) - 32500.0,
%                y = random:uniform(65000.0) - 32500.0,
%                z = random:uniform(65000.0) - 32500.0},
%   C ! {add, spawn(?MODULE, tester, [C, V]),
%              V#vector.x, V#vector.y, V#vector.z},
%    test(N-1, C).
%
%tester(Cell, Vector) ->
%    receive
%    talk ->
%        tester(Cell, Vector, 0)
%    end.
%
%tester(Cell, #vector{x=X, y=Y, z=Z}, Count) ->
%    V = #vector{x=X+uniform:random(),
%                y=Y+uniform:random(),
%                z=Z+uniform:random()},
%    receive
%    {Cell, rpc} ->
%        Cell ! ok,
%        Cell ! {set, self(), V#vector.x, V#vector.y, V#vector.z},
%        tester(Cell, V, Count);
%    {Cell, ok} ->
%        Cell ! {set, self(), V#vector.x, V#vector.y, V#vector.z},
%       tester(Cell, Count+1);
%    {From, die} ->
%        From ! {self(), Count},
%        dead
%    end.
%
%ptest(N) ->
%    C = create(),
%    test(N, C),
%    {C, length(erlang:processes())}.