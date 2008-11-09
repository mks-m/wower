% TODO: this will be oct-tree based process 
%       pool for handling objects in region
-module(cell).
-export([create/0,
         init/1, init/2,
         mmm/7, mmp/7, mpm/7, mpp/7,
         pmm/7, pmp/7, ppm/7, ppp/7]).

% general info about cell
% p - parent cell
% n - navigation record or neighbours record
% l - location of center of the cell
% s - size of cell
-record(info, {p, n, l, s}).

% navigation record for meta-cell
-record(navigation, { mmm, mmp, mpm, mpp,
                      pmm, pmp, ppm, ppp }).

% size, location and object records
-record(vector, {x, y, z}).

-define(MAX_PER_CELL, 5).
-define(MIN_CELL_SIZE, 500).

% used to create root node
create() ->
    Info = #info{s=#vector{x=65000, y=65000, z=65000}, 
                 l=#vector{x=0.0, y=0.0, z=0.0}},
    spawn_link(?MODULE, init, [Info]).

% used internally to create child nodes while splitting cell 
create(Bitmap, #info{s=#vector{x=SX, y=SY, z=SZ}, 
                     l=#vector{x=LX, y=LY, z=LZ}}, O) ->
    NS = #vector{x=SX/2, y=SY/2, z=SZ/2},
    {NL, NO} = ?MODULE:Bitmap(O, SX, SY, SZ, LX, LY, LZ),
    spawn_link(?MODULE, init, [#info{p=self(), s=NS, l=NL}, NO]).

% initializes root node
init(#info{} = Info) ->
    io:format("node started:~nsize: ~p~nlocn: ~p~n", [Info#info.s, Info#info.l]),
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
        bc_up(Info, ObjectLocation, Range, Message),
        InRange = bc_inrange(all, ObjectLocation, Range, Objects),
        dict:fold(fun(K, _, ok) -> K ! Message, ok end, ok, InRange),
        cell(Info, Objects);
    {status, Parent} ->
        io:format("cell, holding ~p~n", [dict:size(Objects)]),
        cell(Info, Objects);
    {die, Parent} ->
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
        Index = compare(X, (Info#info.l)#vector.x)*4 +
                compare(X, (Info#info.l)#vector.x)*2 +
                compare(X, (Info#info.l)#vector.x)*1 + 2,
        erlang:element(Index, Info#info.n) ! {add, ObjectPid, X, Y, Z},
        meta(Info);
    {set, ObjectPid, X, Y, Z} ->
        Index = compare(X, (Info#info.l)#vector.x)*4 +
                compare(X, (Info#info.l)#vector.x)*2 +
                compare(X, (Info#info.l)#vector.x)*1 + 2,
        erlang:element(Index, Info#info.n) ! {set, ObjectPid, X, Y, Z},
        meta(Info);
    {bcm, Parent, ObjectLocation, Range, Message} ->
        bc_down(Info, ObjectLocation, Range, Message),
        meta(Info);
    {bcc, From, ObjectLocation, Range, Message} ->
        bc_up(Info, ObjectLocation, Range, Message),
        bc_down(Info, From, ObjectLocation, Range, Message),
        meta(Info);
    {status, Parent} ->
        io:format("meta, children: ~n"),
        N = Info#info.n,
        N#navigation.mmm ! {status, self()},
        N#navigation.mmp ! {status, self()},
        N#navigation.mpm ! {status, self()},
        N#navigation.mpp ! {status, self()},
        N#navigation.pmm ! {status, self()},
        N#navigation.pmp ! {status, self()},
        N#navigation.ppm ! {status, self()},
        N#navigation.ppp ! {status, self()},
        meta(Info);
    {die, Parent} ->
        N = Info#info.n,
        N#navigation.mmm ! {die, self()},
        N#navigation.mmp ! {die, self()},
        N#navigation.mpm ! {die, self()},
        N#navigation.mpp ! {die, self()},
        N#navigation.pmm ! {die, self()},
        N#navigation.pmp ! {die, self()},
        N#navigation.ppm ! {die, self()},
        N#navigation.ppp ! {die, self()},
        ok;
    _ ->
        meta(Info)
    end.

split(#info{s=#vector{x=X, y=Y, z=Z}} = Info, Objects) when X/4+Y/4+Z/4 >= ?MIN_CELL_SIZE ->
    io:format("going to split~n"),
    Navigation = #navigation{mmm=create(mmm, Info, Objects),
                             mmp=create(mmp, Info, Objects),
                             mpm=create(mpm, Info, Objects),
                             mpp=create(mpp, Info, Objects),
                             pmm=create(pmm, Info, Objects),
                             pmp=create(pmp, Info, Objects),
                             ppm=create(ppm, Info, Objects),
                             ppp=create(ppp, Info, Objects)},
    meta(Info#info{n=Navigation});
split(Info, Objects) ->
    io:format("cell overloaded~n"),
    cell(Info, Objects).

mmm(O, SX, SY, SZ, LX, LY, LZ) ->
    {#vector{x=LX-SX/4, y=LY-SY/4, z=LZ-SZ/4},
     dict:filter(fun(_, #vector{x=X, y=Y, z=Z}) -> 
                     if X < SX andalso Y < SY andalso Z < SZ -> true; 
                     true -> false end 
                 end, O)}.

mmp(O, SX, SY, SZ, LX, LY, LZ) ->
    {#vector{x=LX-SX/4, y=LY-SY/4, z=LZ+SZ/4},
     dict:filter(fun(_, #vector{x=X, y=Y, z=Z}) -> 
                     if X < SX andalso Y < SY andalso Z >= SZ -> true; 
                     true -> false end 
                 end, O)}.

mpm(O, SX, SY, SZ, LX, LY, LZ) ->
    {#vector{x=LX-SX/4, y=LY+SY/4, z=LZ-SZ/4},
     dict:filter(fun(_, #vector{x=X, y=Y, z=Z}) -> 
                     if X < SX andalso Y >= SY andalso Z < SZ -> true; 
                     true -> false end 
                 end, O)}.

mpp(O, SX, SY, SZ, LX, LY, LZ) ->
    {#vector{x=LX-SX/4, y=LY+SY/4, z=LZ+SZ/4},
     dict:filter(fun(_, #vector{x=X, y=Y, z=Z}) -> 
                     if X < SX andalso Y >= SY andalso Z >= SZ -> true; 
                     true -> false end 
                 end, O)}.

pmm(O, SX, SY, SZ, LX, LY, LZ) ->
    {#vector{x=LX+SX/4, y=LY-SY/4, z=LZ-SZ/4},
     dict:filter(fun(_, #vector{x=X, y=Y, z=Z}) -> 
                     if X >= SX andalso Y < SY andalso Z < SZ -> true; 
                     true -> false end 
                 end, O)}.

pmp(O, SX, SY, SZ, LX, LY, LZ) ->
    {#vector{x=LX+SX/4, y=LY-SY/4, z=LZ+SZ/4},
     dict:filter(fun(_, #vector{x=X, y=Y, z=Z}) -> 
                     if X >= SX andalso Y < SY andalso Z >= SZ -> true; 
                     true -> false end 
                 end, O)}.

ppm(O, SX, SY, SZ, LX, LY, LZ) ->
    {#vector{x=LX+SX/4, y=LY+SY/4, z=LZ-SZ/4},
     dict:filter(fun(_, #vector{x=X, y=Y, z=Z}) -> 
                     if X >= SX andalso Y >= SY andalso Z < SZ -> true; 
                     true -> false end 
                 end, O)}.

ppp(O, SX, SY, SZ, LX, LY, LZ) ->
    {#vector{x=LX+SX/4, y=LY+SY/4, z=LZ+SZ/4},
     dict:filter(fun(_, #vector{x=X, y=Y, z=Z}) -> 
                     if X >= SX andalso Y >= SY andalso Z >= SZ -> true; 
                     true -> false end 
                 end, O)}.

compare(X, Y) when X < Y -> 0;
compare(_, _) -> 1.

bc_up(Info, #vector{x=OX, y=OY, z=OZ} = O, #vector{x=RX, y=RY, z=RZ} = R, Message) ->
    #info{l=#vector{x=LX, y=LY, z=LZ},
          s=#vector{x=SX, y=SY, z=SZ}} = Info,
    if LX-SX <  OX-RX orelse LY-SY <  OY-RY orelse LZ-SZ <  OZ-RZ orelse
       LX+SX >= OX+RX orelse LY+SY >= OY+RY orelse LZ+SZ >= OZ+RZ ->
        Info#info.p ! {bc, self(), O, R, Message},
        ok;
    true ->
        ok
    end.

bc_inrange(From, #vector{x=OX, y=OY, z=OZ}, #vector{x=RX, y=RY, z=RZ}, Objects) ->
    dict:filter(fun(K, #vector{x=KX, y=KY, z=KZ}) ->
                    if From =/= K andalso
                       KX < OX+RX andalso KX > OX-RX andalso
                       KY < OY+RY andalso KY > OY-RY andalso
                       KZ < OZ+RZ andalso KZ > OZ-RZ -> true;
                    true -> false
                    end 
                end, Objects).

bc_down(Info, Object, Range, Message) ->
    ok.

bc_down(Info, Except, Object, Range, Message) ->
    ok.
