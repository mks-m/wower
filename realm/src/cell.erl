% TODO: this will be oct-tree based process 
%       pool for handling objects in region
-module(cell).
-export([start/0, stop/0, world/0, create/0, init/1, init/2]).
-export([test/2, ptest/1, tester/2]).

% size, location and object records
%% @type vector() = {vector, float(), float(), float()}
-record(vector, {x, y, z}).

% general info about cell
% p - parent cell
% n - navigation record or neighbours record
% l - location of center of the cell
% s - size of cell
%% @type info() = {info, pid() | undefined, tuple() | undefined, vector(), float()}
-record(info, {p, n, l, s}).

-define(MAX_PER_CELL, 1000).
-define(MIN_CELL_SIZE, 500).

%% @spec start() -> ok.
start() ->
    Pid = spawn(?MODULE, world, []),
    register(world, Pid),
    ok.

%% @spec world() -> ok.
world() ->
    Maps = dict:from_list([{  0, create()},   % Eastern Kingdoms
                           {  1, create()},   % Kalimdor
                           {501, create()},   % Outland
                           {571, create()}]), % Northrend
    world(Maps).

%% @spec world(dictionary()) -> ok.
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

%% @spec stop() -> ok.
stop() ->
    world ! stop,
    ok.

% used to create root node
%% @spec create() -> pid().
create() ->
    Info = #info{s=65000.0, l=#vector{x=0.0, y=0.0, z=0.0}},
    spawn_link(?MODULE, init, [Info]).

% used internally to create child nodes while splitting cell
%% @spec create(int(), info()) -> pid().
create(Bitmap, #info{s=S, l=#vector{x=LX, y=LY, z=LZ}}, O) ->
    {NL, NO} = filter(Bitmap, O, S, LX, LY, LZ),
    spawn_link(?MODULE, init, [#info{p=self(), s=S/2, l=NL}, NO]).

% initializes root node
%% @spec init(info()) -> ok.
init(#info{} = Info) ->
    Objects = dict:new(),
    cell(Info, Objects).

% initializes child cell with predefined objects count
%% @spec start(info(), dictionary()) -> ok.
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
%% @spec cell(info(), dictionary()) -> ok.
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
    
    status ->
        io:format("- cell @ (~.2f, ~.2f, ~.2f), size: ~p, holding: ~p~n", [(Info#info.l)#vector.x, 
                                                                           (Info#info.l)#vector.y, 
                                                                           (Info#info.l)#vector.z,
                                                                           Info#info.s, dict:size(Objects)]),
        cell(Info, Objects);

    {status, Pid, Offset} ->
        io:format(Offset ++ "- cell @ (~.2f, ~.2f, ~.2f), size: ~p, holding: ~p~n", [(Info#info.l)#vector.x, 
                                                                                     (Info#info.l)#vector.y, 
                                                                                     (Info#info.l)#vector.z,
                                                                                     Info#info.s, dict:size(Objects)]),
        Pid ! {status, self(), ok},
        cell(Info, Objects);
    
    die ->
        ok;
    {die, Pid} ->
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
%% @spec meta(info()) -> ok.
meta(#info{p=Parent} = Info) ->
    receive
    {add, ObjectPid, X, Y, Z} ->
        Index = index(#vector{x=X, y=Y, z=Z}, Info#info.l),
        erlang:element(Index, Info#info.n) ! {add, ObjectPid, X, Y, Z},
        meta(Info);
    {set, ObjectPid, X, Y, Z} ->
        Index = index(#vector{x=X, y=Y, z=Z}, Info#info.l),
        erlang:element(Index, Info#info.n) ! {set, ObjectPid, X, Y, Z},
        meta(Info);

    {bcm, Parent, ObjectLocation, Range, Message} ->
        bc_down(Info, ObjectLocation, Range, Message),
        meta(Info);
    {bcc, From, ObjectLocation, Range, Message} ->
        bc_up(Info, ObjectLocation, Range, Message),
        bc_down(Info, From, ObjectLocation, Range, Message),
        meta(Info);
    
    status ->
        io:format("meta, size: ~p~n", [Info#info.s]),
        N = Info#info.n,
        rpc(element(1, N), {status, self(), "    "}),
        rpc(element(2, N), {status, self(), "    "}),
        rpc(element(3, N), {status, self(), "    "}),
        rpc(element(4, N), {status, self(), "    "}),
        rpc(element(5, N), {status, self(), "    "}),
        rpc(element(6, N), {status, self(), "    "}),
        rpc(element(7, N), {status, self(), "    "}),
        rpc(element(8, N), {status, self(), "    "}),
        meta(Info);
        

    {status, Pid, Offset} ->
        io:format(Offset ++ "- meta, size: ~p~n", [Info#info.s]),
        N = Info#info.n,
        rpc(element(1, N), {status, self(), Offset ++ "    "}),
        rpc(element(2, N), {status, self(), Offset ++ "    "}),
        rpc(element(3, N), {status, self(), Offset ++ "    "}),
        rpc(element(4, N), {status, self(), Offset ++ "    "}),
        rpc(element(5, N), {status, self(), Offset ++ "    "}),
        rpc(element(6, N), {status, self(), Offset ++ "    "}),
        rpc(element(7, N), {status, self(), Offset ++ "    "}),
        rpc(element(8, N), {status, self(), Offset ++ "    "}),
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

%% @spec split(info(), dictionary()) -> ok.
split(#info{s=S} = Info, Objects) when S/2 >= ?MIN_CELL_SIZE ->
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

%% @spec filter(int(), dictionary(), float(), float(), float(), float()) -> {vector(), dictionary()}.
filter(1, O, S, LX, LY, LZ) ->
    {#vector{x=LX-S/4, y=LY-S/4, z=LZ-S/4},
     dict:filter(fun(_, #vector{x=X, y=Y, z=Z}) -> 
                     if X < LX andalso Y < LY andalso Z < LZ -> true; 
                     true -> false end 
                 end, O)};
filter(2, O, S, LX, LY, LZ) ->
    {#vector{x=LX-S/4, y=LY-S/4, z=LZ+S/4},
     dict:filter(fun(_, #vector{x=X, y=Y, z=Z}) -> 
                     if X < LX andalso Y < LY andalso Z >= LZ -> true; 
                     true -> false end 
                 end, O)};
filter(3, O, S, LX, LY, LZ) ->
    {#vector{x=LX-S/4, y=LY+S/4, z=LZ-S/4},
     dict:filter(fun(_, #vector{x=X, y=Y, z=Z}) -> 
                     if X < LX andalso Y >= LY andalso Z < LZ -> true; 
                     true -> false end 
                 end, O)};
filter(4, O, S, LX, LY, LZ) ->
    {#vector{x=LX-S/4, y=LY+S/4, z=LZ+S/4},
     dict:filter(fun(_, #vector{x=X, y=Y, z=Z}) -> 
                     if X < LX andalso Y >= LY andalso Z >= LZ -> true; 
                     true -> false end 
                 end, O)};
filter(5, O, S, LX, LY, LZ) ->
    {#vector{x=LX+S/4, y=LY-S/4, z=LZ-S/4},
     dict:filter(fun(_, #vector{x=X, y=Y, z=Z}) -> 
                     if X >= LX andalso Y < LY andalso Z < LZ -> true; 
                     true -> false end 
                 end, O)};
filter(6, O, S, LX, LY, LZ) ->
    {#vector{x=LX+S/4, y=LY-S/4, z=LZ+S/4},
     dict:filter(fun(_, #vector{x=X, y=Y, z=Z}) -> 
                     if X >= LX andalso Y < LY andalso Z >= LZ -> true; 
                     true -> false end 
                 end, O)};
filter(7, O, S, LX, LY, LZ) ->
    {#vector{x=LX+S/4, y=LY+S/4, z=LZ-S/4},
     dict:filter(fun(_, #vector{x=X, y=Y, z=Z}) -> 
                     if X >= LX andalso Y >= LY andalso Z < LZ -> true; 
                     true -> false end 
                 end, O)};
filter(8, O, S, LX, LY, LZ) ->
    {#vector{x=LX+S/4, y=LY+S/4, z=LZ+S/4},
     dict:filter(fun(_, #vector{x=X, y=Y, z=Z}) -> 
                     if X >= LX andalso Y >= LY andalso Z >= LZ -> true; 
                     true -> false end 
                 end, O)}.

%% @spec compare(float(), float()) -> 0 | 1.
compare(X, Y) when X < Y -> 0;
compare(_, _) -> 1.

%% @spec index(vector(), vector()) -> int().
index(#vector{x=X1, y=Y1, z=Z1}, 
      #vector{x=X2, y=Y2, z=Z2}) ->
    index(compare(X1, X2), compare(Y1, Y2), compare(Z1, Z2)).

%% @spec index(int(), int(), int()) -> int().
index(X, Y, Z) ->
    X * 4 + Y * 2 + Z + 1.

%% @spec bc_up(info(), vector(), float(), any()) -> ok.
bc_up(Info, #vector{x=OX, y=OY, z=OZ} = O, R, Message) ->
    #info{l=#vector{x=LX, y=LY, z=LZ}, s=S} = Info,
    if LX-S <  OX-R orelse LY-S <  OY-R orelse LZ-S <  OZ-R orelse
       LX+S >= OX+R orelse LY+S >= OY+R orelse LZ+S >= OZ+R ->
        Info#info.p ! {bcc, self(), O, R, Message},
        ok;
    true ->
        ok
    end.

%% @spec bc_inrange(pid(), vector(), float(), dictionary()) -> dictionary().
bc_inrange(From, O1, R, Objects) ->
    dict:filter(fun(K, O2) ->
                    D = d(O1, O2),
                    if From =/= K andalso D =< R -> true;
                                            true -> false
                    end 
                end, Objects).

%% @spec bc_down(info(), vector(), float(), any()) -> ok.
bc_down(Info, Object, R, Message) ->
    M = {bcm, self(), Object, R, Message},
    { P1, P2, P3, P4, P5, P6, P7, P8 } = Info#info.n,
    { R1, R2, R3, R4, R5, R6, R7, R8} = bc_down_cells(Info#info.l, Info#info.s, Object),
    if R1 =< R -> P1 ! M; true -> ok end,
    if R2 =< R -> P2 ! M; true -> ok end,
    if R3 =< R -> P3 ! M; true -> ok end,
    if R4 =< R -> P4 ! M; true -> ok end,
    if R5 =< R -> P5 ! M; true -> ok end,
    if R6 =< R -> P6 ! M; true -> ok end,
    if R7 =< R -> P7 ! M; true -> ok end,
    if R8 =< R -> P8 ! M; true -> ok end,
    ok.

%% @spec bc_down(info(), pid(), vector(), float(), any()) -> ok.
bc_down(Info, Except, Object, R, Message) ->
    M = {bcm, self(), Object, R, Message},
    { P1, P2, P3, P4, P5, P6, P7, P8 } = Info#info.n,
    { R1, R2, R3, R4, R5, R6, R7, R8 } = bc_down_cells(Info#info.l, Info#info.s, Object),
    if P1 =/= Except andalso R1 =< R -> P1 ! M; true -> ok end,
    if P1 =/= Except andalso R2 =< R -> P2 ! M; true -> ok end,
    if P1 =/= Except andalso R3 =< R -> P3 ! M; true -> ok end,
    if P1 =/= Except andalso R4 =< R -> P4 ! M; true -> ok end,
    if P1 =/= Except andalso R5 =< R -> P5 ! M; true -> ok end,
    if P1 =/= Except andalso R6 =< R -> P6 ! M; true -> ok end,
    if P1 =/= Except andalso R7 =< R -> P7 ! M; true -> ok end,
    if P1 =/= Except andalso R8 =< R -> P8 ! M; true -> ok end,
    ok.

%% @spec bc_down_cells(vector(), float(), vector()) -> ok.
bc_down_cells(#vector{x=LX, y=LY, z=LZ}, S, Object) ->
    {d(#vector{x=LX-S/4, y=LY-S/4, z=LZ-S/4}, Object),
     d(#vector{x=LX-S/4, y=LY-S/4, z=LZ+S/4}, Object),
     d(#vector{x=LX-S/4, y=LY+S/4, z=LZ-S/4}, Object),
     d(#vector{x=LX-S/4, y=LY+S/4, z=LZ+S/4}, Object),
     d(#vector{x=LX+S/4, y=LY-S/4, z=LZ-S/4}, Object),
     d(#vector{x=LX+S/4, y=LY-S/4, z=LZ+S/4}, Object),
     d(#vector{x=LX+S/4, y=LY+S/4, z=LZ-S/4}, Object),
     d(#vector{x=LX+S/4, y=LY+S/4, z=LZ+S/4}, Object)}.

%% @spec d(vector(), vector()) -> float().
d(#vector{x=X1, y=Y1, z=Z1},
  #vector{x=X2, y=Y2, z=Z2}) ->
    DX = X1-X2, DY = Y1-Y2, DZ = Z1-Z2,
    math:sqrt(DX*DX + DY*DY + DZ*DZ).

%% @spec rpc(pid(), any()) -> any().
rpc(C, M) ->
    C ! M,
    receive 
        V -> V
    end.
