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

