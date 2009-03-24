% TODO: this will be oct-tree based process 
%       pool for handling objects in region
-module(cell).
-export([start/0, stop/0, world/0, create/0, test/1, init/1, init/2]).

% general info about cell
% p - parent cell
% n - navigation record or neighbours record
% l - location of center of the cell
% s - size of cell
-record(info, {p, n, l, s}).

% size, location and object records
-record(vector, {x, y, z}).

-define(MAX_PER_CELL, 5).
-define(MIN_CELL_SIZE, 500).

start() ->
    Pid = spawn(?MODULE, world, []),
    register(world, Pid),
    ok.

world() ->
    Maps = dict:from_list([{  0, create()},   % Eastern Kingdoms
                           {  1, create()},   % Kalimdor
                           {501, create()},   % Outland
                           {571, create()}]), % Northrend
    world(Maps).

world(Maps) ->
    receive
    {From, find, MapId} ->
        {ok, MapPid} = dict:find(MapId, Maps),
        From ! {world, found, MapPid},
        world(Maps);
    stop ->
        dict:map(fun(_,V) -> V ! {die, undefined} end, Maps),
        ok;
    _ ->
        world(Maps)
    end.


stop() ->
    world ! stop,
    ok.

% used to create root node
create() ->
    Info = #info{s=#vector{x=65000, y=65000, z=65000}, 
                 l=#vector{x=0.0, y=0.0, z=0.0}},
    spawn_link(?MODULE, init, [Info]).

% used internally to create child nodes while splitting cell 
create(Bitmap, #info{s=#vector{x=SX, y=SY, z=SZ}, 
                     l=#vector{x=LX, y=LY, z=LZ}}, O) ->
    NS = #vector{x=SX/2, y=SY/2, z=SZ/2},
    {NL, NO} = filter(Bitmap, O, SX, SY, SZ, LX, LY, LZ),
    spawn_link(?MODULE, init, [#info{p=self(), s=NS, l=NL}, NO]).

% initializes root node
init(#info{} = Info) ->
    Objects = dict:new(),
    cell(Info, Objects).

% initializes child cell with predefined objects count
init(#info{} = Info, Objects) ->
    cell(Info, Objects).

% main loop for cell
% accepts next messages:
%   add: adds object to keep an eye on
%        also takes care of splitting cell into 8 more if 
%        there's enough amount of objects added
%
%   set: updates object's coordinates
%
%   bco: broadcasts message from one object to others in 
%        specified range also takes care of inter-cell 
%        broadcasting when range goes out of cell bounds
%
%   bcm: broadcast message from meta cell. this can happen
%        when cell is included in range of message received 
%        by neibourgh cell
%
%   status: used for testing purposes
%
%   die: dispose objects storage and end process
cell(#info{p=Parent} = Info, Objects) ->
    receive
    {set, ObjectPid, X, Y, Z} ->
        NewObjects = dict:store(ObjectPid, #vector{x=X, y=Y, z=Z}, Objects),
        cell(Info, NewObjects);
    {add, ObjectPid, X, Y, Z} ->
        NewObjects = dict:store(ObjectPid, #vector{x=X, y=Y, z=Z}, Objects),
        Count = dict:size(Objects),
        if Count > ?MAX_PER_CELL -> split(Info, NewObjects);
                            true -> cell(Info, NewObjects)
        end;
    
    {bco, From, ObjectLocation, Range, Message} ->
        bc_up(Info, ObjectLocation, Range, Message),
        InRange = bc_inrange(From, ObjectLocation, Range, Objects),
        dict:fold(fun(K, _, ok) -> K ! Message, ok end, ok, InRange),
        cell(Info, Objects);
    {bcm, Parent, ObjectLocation, Range, Message} ->
        InRange = bc_inrange(all, ObjectLocation, Range, Objects),
        dict:fold(fun(K, _, ok) -> K ! Message, ok end, ok, InRange),
        cell(Info, Objects);
    
    {status, undefined} ->
        io:format("cell, holding ~p~n", [dict:size(Objects)]),
        cell(Info, Objects);
    {status, Pid} ->
        io:format("cell, holding ~p~n", [dict:size(Objects)]),
        Pid ! {status, self(), ok},
        cell(Info, Objects);
    
    {die, undefined} ->
        io:format("dead~n"),
        ok;
    {die, Pid} ->
        io:format("dead~n"),
        Pid ! {status, self(), ok},
        ok;
    
    _ ->
        cell(Info, Objects)
    end.

% main loop for cell
% accepts next messages:
%   add: adds object to keep an eye on
%        also takes care of splitting cell into 8 more if 
%        there's enough amount of objects added
%
%   set: updates object's coordinates
%
%   bcm: broadcast message from meta cell. this can happen
%        when cell is included in range of message received 
%        by neibourgh cell
%
%   bcc: broadcast message from child cell. this can happen
%        when received message range goes out of cell space 
%
%   status: used for testing purposes
%
%   die: dispose objects storage and end process
% TODO: merge meta-cell into one cell
meta(#info{p=Parent} = Info) ->
    receive
    {add, ObjectPid, X, Y, Z} ->
        Index = index(#vector{x=X, y=Y, z=Z}, (Info#info.l)#vector.x),
        io:format("put into ~p~n", [Index]),
        erlang:element(Index, Info#info.n) ! {add, ObjectPid, X, Y, Z},
        meta(Info);
    {set, ObjectPid, X, Y, Z} ->
        Index = index(#vector{x=X, y=Y, z=Z}, (Info#info.l)#vector.x),
        erlang:element(Index, Info#info.n) ! {set, ObjectPid, X, Y, Z},
        meta(Info);

    {bcm, Parent, ObjectLocation, Range, Message} ->
        bc_down(Info, ObjectLocation, Range, Message),
        meta(Info);
    {bcc, From, ObjectLocation, Range, Message} ->
        bc_up(Info, ObjectLocation, Range, Message),
        bc_down(Info, From, ObjectLocation, Range, Message),
        meta(Info);
    
    {status, Pid} ->
        io:format("meta, children: ~n"),
        N = Info#info.n,
        rpc(element(1, N), status),
        rpc(element(2, N), status),
        rpc(element(3, N), status),
        rpc(element(4, N), status),
        rpc(element(5, N), status),
        rpc(element(6, N), status),
        rpc(element(7, N), status),
        rpc(element(8, N), status),
        io:format("meta end~n~n"),
        Pid ! {status, self(), ok},
        meta(Info);

    {die, Pid} ->
        io:format("meta, killing children: ~n"),
        N = Info#info.n,
        rpc(element(1, N), die),
        rpc(element(2, N), die),
        rpc(element(3, N), die),
        rpc(element(4, N), die),
        rpc(element(5, N), die),
        rpc(element(6, N), die),
        rpc(element(7, N), die),
        rpc(element(8, N), die),
        Pid ! {self(), die, ok},
        ok;

    _ ->
        meta(Info)
    end.

split(#info{s=#vector{x=X, y=Y, z=Z}} = Info, Objects) when X/4+Y/4+Z/4 >= ?MIN_CELL_SIZE ->
    io:format("going to split~n"),
    Navigation = {create(1, Info, Objects),
                  create(2, Info, Objects),
                  create(3, Info, Objects),
                  create(4, Info, Objects),
                  create(5, Info, Objects),
                  create(6, Info, Objects),
                  create(7, Info, Objects),
                  create(8, Info, Objects)},
    meta(Info#info{n=Navigation});
split(Info, Objects) ->
    io:format("cell overloaded~n"),
    cell(Info, Objects).

filter(1, O, SX, SY, SZ, LX, LY, LZ) ->
    {#vector{x=LX-SX/4, y=LY-SY/4, z=LZ-SZ/4},
     dict:filter(fun(_, #vector{x=X, y=Y, z=Z}) -> 
                     if X < LX andalso Y < LY andalso Z < LZ -> true; 
                     true -> false end 
                 end, O)};
filter(2, O, SX, SY, SZ, LX, LY, LZ) ->
    {#vector{x=LX-SX/4, y=LY-SY/4, z=LZ+SZ/4},
     dict:filter(fun(_, #vector{x=X, y=Y, z=Z}) -> 
                     if X < LX andalso Y < LY andalso Z >= LZ -> true; 
                     true -> false end 
                 end, O)};
filter(3, O, SX, SY, SZ, LX, LY, LZ) ->
    {#vector{x=LX-SX/4, y=LY+SY/4, z=LZ-SZ/4},
     dict:filter(fun(_, #vector{x=X, y=Y, z=Z}) -> 
                     if X < LX andalso Y >= LY andalso Z < LZ -> true; 
                     true -> false end 
                 end, O)};
filter(4, O, SX, SY, SZ, LX, LY, LZ) ->
    {#vector{x=LX-SX/4, y=LY+SY/4, z=LZ+SZ/4},
     dict:filter(fun(_, #vector{x=X, y=Y, z=Z}) -> 
                     if X < LX andalso Y >= LY andalso Z >= LZ -> true; 
                     true -> false end 
                 end, O)};
filter(5, O, SX, SY, SZ, LX, LY, LZ) ->
    {#vector{x=LX+SX/4, y=LY-SY/4, z=LZ-SZ/4},
     dict:filter(fun(_, #vector{x=X, y=Y, z=Z}) -> 
                     if X >= LX andalso Y < LY andalso Z < LZ -> true; 
                     true -> false end 
                 end, O)};
filter(6, O, SX, SY, SZ, LX, LY, LZ) ->
    {#vector{x=LX+SX/4, y=LY-SY/4, z=LZ+SZ/4},
     dict:filter(fun(_, #vector{x=X, y=Y, z=Z}) -> 
                     if X >= LX andalso Y < LY andalso Z >= LZ -> true; 
                     true -> false end 
                 end, O)};
filter(7, O, SX, SY, SZ, LX, LY, LZ) ->
    {#vector{x=LX+SX/4, y=LY+SY/4, z=LZ-SZ/4},
     dict:filter(fun(_, #vector{x=X, y=Y, z=Z}) -> 
                     if X >= LX andalso Y >= LY andalso Z < LZ -> true; 
                     true -> false end 
                 end, O)};
filter(8, O, SX, SY, SZ, LX, LY, LZ) ->
    {#vector{x=LX+SX/4, y=LY+SY/4, z=LZ+SZ/4},
     dict:filter(fun(_, #vector{x=X, y=Y, z=Z}) -> 
                     if X >= LX andalso Y >= LY andalso Z >= LZ -> true; 
                     true -> false end 
                 end, O)}.

compare(X, Y) when X < Y -> 0;
compare(_, _) -> 1.

index(#vector{x=X1, y=Y1, z=Z1}, 
      #vector{x=X2, y=Y2, z=Z2}) ->
    index(compare(X1, X2), compare(Y1, Y2), compare(Z1, Z2)).

index(X, Y, Z) ->
    X * 4 + Y * 2 + Z + 1.

bc_up(Info, #vector{x=OX, y=OY, z=OZ} = O, R, Message) ->
    #info{l=#vector{x=LX, y=LY, z=LZ},
          s=#vector{x=SX, y=SY, z=SZ}} = Info,
    if LX-SX <  OX-R orelse LY-SY <  OY-R orelse LZ-SZ <  OZ-R orelse
       LX+SX >= OX+R orelse LY+SY >= OY+R orelse LZ+SZ >= OZ+R ->
        Info#info.p ! {bcc, self(), O, R, Message},
        ok;
    true ->
        ok
    end.

bc_inrange(From, #vector{x=OX, y=OY, z=OZ}, R, Objects) ->
    dict:filter(fun(K, #vector{x=KX, y=KY, z=KZ}) ->
                    if From =/= K andalso
                       KX < OX+R andalso KX > OX-R andalso
                       KY < OY+R andalso KY > OY-R andalso
                       KZ < OZ+R andalso KZ > OZ-R ->
                        true;
                    true ->
                        false
                    end 
                end, Objects).

bc_down(Info, Object, Range, Message) ->
    M = {bcm, self(), Object, Range, Message},
    { P1, P2, P3, P4,
      P5, P6, P7, P8 } = Info#info.n,
    P1 ! M, P2 ! M, P3 ! M, P4 ! M,
    P5 ! M, P6 ! M, P7 ! M, P8 ! M,
    ok.

bc_down(Info, Except, Object, Range, Message) ->
    M = {bcm, self(), Object, Range, Message},
    { P1, P2, P3, P4,
      P5, P6, P7, P8 } = Info#info.n,
    if P1 =/= Except -> P1 ! M; true -> ok end,
    if P2 =/= Except -> P2 ! M; true -> ok end,
    if P3 =/= Except -> P3 ! M; true -> ok end,
    if P4 =/= Except -> P4 ! M; true -> ok end,
    if P5 =/= Except -> P5 ! M; true -> ok end,
    if P6 =/= Except -> P6 ! M; true -> ok end,
    if P7 =/= Except -> P7 ! M; true -> ok end,
    if P8 =/= Except -> P8 ! M; true -> ok end,
    ok.

rpc(C, M) ->
    S = self(),
    C ! {M, S},
    receive 
        {M, C, V} -> V
    end.

test(N) ->
    C = cell:create(),
    test(N, C),
    C ! {bcm, undefined, #vector{x=0, y=0, z=0}, 32500, talk}.

test(0, C) ->
    C;
test(N, C) ->
    V = #vector{x = random:uniform(65000),
                y = random:uniform(65000),
                z = random:uniform(65000)},
    C ! {add, spawn(?MODULE, tester, [C, V]),
              V#vector.x, V#vector.y, V#vector.z},
    test(N-1, C).

tester(Cell, Vector) ->
    receive
    talk ->
        tester(Cell, Vector, 0)
    end.

tester(Cell, #vector{x=X, y=Y, z=Z}, Count) ->
    V = #vector{x=X+uniform:random(),
                y=Y+uniform:random(),
                z=Z+uniform:random()},
    receive
    {Cell, rpc} ->
        Cell ! ok,
        Cell ! {set, self(), V#vector.x, V#vector.y, V#vector.z},
        tester(Cell, V, Count);
    {Cell, ok} ->
        Cell ! {set, self(), V#vector.x, V#vector.y, V#vector.z},
        tester(Cell, Count+1);
    {From, die} ->
        From ! {self(), Count},
        dead
    end.

wait(N) ->
    receive after N -> ok end.
