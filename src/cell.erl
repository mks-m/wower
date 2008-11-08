% TODO: this will be oct-tree based process 
%       pool for handling objects in region
-module(cell).
-export([create/0, create/3,
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
-record(location, {x, y, z}).
-record(size, {x, y, z}).
-record(object, {o, x, y, z}).

-define(MAX_PER_CELL, 1000).

create() ->
    Info = #info{s=#size{x=math:pow(2, 16),
                         y=math:pow(2, 16),
                         z=math:pow(2, 16)}, 
                 l=#location{x=0.0, 
                             y=0.0, 
                             z=0.0}},
    spawn_link(?MODULE, init, [Info]).

create(Bitmap, #info{s=#size{x=SX, y=SY, z=SZ}, 
                     l=#location{x=LX, y=LY, z=LZ}}, O) ->
    NS = #size{x=SX/2, y=SY/2, z=SZ/2},
    {NL, NO} = ?MODULE:Bitmap(O, SX, SY, SZ, LX, LY, LZ),
    spawn_link(?MODULE, init, [#info{p=self(), s=NS, l=NL}, NO]).

init(#info{} = Info) ->
    Objects = ets:new(objects, [set, private, {keypos, 2}]),
    cell(Info, Objects).

init(#info{} = Info, ObjectsList) ->
    Objects = ets:new(objects, [set, private, {keypos, 2}]),
    ets:insert(Objects, ObjectsList),
    cell(Info, Objects).

cell(Info, Objects) ->
    receive
    {add, O, X, Y, Z} ->
        ets:insert_new(Objects, #object{o=O, x=X, y=Y, z=Z}),
        Count = ets:select_count(Objects, [{'_'}]), 
        if 
        Count > ?MAX_PER_CELL ->
            split(Info, Objects);
        true ->
            cell(Info, Objects)
        end;
    {set, O, X, Y, Z} ->
        ets:insert(Objects, #object{o=O, x=X, y=Y, z=Z}),
        cell(Info, Objects);
    {bc, #object{x=OX, y=OY, z=OZ} = O, #size{x=RX, y=RY, z=RZ} = R, Message} ->
        % TODO: implement broadcasting to objects
        #info{l=#location{x=LX, y=LY, z=LZ},
              s=#size{x=SX, y=SY, z=SZ}} = Info,
        if LX-SX <  OX-RX orelse LY-SY <  OY-RY orelse LZ-SZ <  OZ-RZ orelse
           LX+SX >= OX+RX orelse LY+SY >= OY+RY orelse LZ+SZ >= OZ+RZ ->
            Info#info.p ! {bc, self(), O, R, Message};
        true ->
            ok
        end,
        InRange = ets:select(Objects, [{'<', '$3', OX+RX}, {'>', '$3', OX-RX},
                                       {'<', '$3', OY+RY}, {'>', '$3', OY-RY},
                                       {'<', '$3', OZ+RZ}, {'>', '$3', OZ-RZ}]),
        cell(Info, Objects);
    _ ->
        cell(Info, Objects)
    end.

% TODO: merge meta-cell into one cell
meta(Info) ->
    receive
    _ ->
        meta(Info)
    end.

split(Info, Objects) ->
    Navigation = #navigation{mmm=create(mmm, Info, Objects),
                             mmp=create(mmp, Info, Objects),
                             mpm=create(mpm, Info, Objects),
                             mpp=create(mpp, Info, Objects),
                             pmm=create(pmm, Info, Objects),
                             pmp=create(pmp, Info, Objects),
                             ppm=create(ppm, Info, Objects),
                             ppp=create(ppp, Info, Objects)},
    ets:delete(Objects),
    meta(Info#info{n=Navigation}).

mmm(O, SX, SY, SZ, LX, LY, LZ) ->
    {#location{x=LX-SX/4, y=LY-SY/4, z=LZ-SZ/4},
     ets:select(O, [{'<', '$3', SX}, {'<', '$4', SY}, {'<', '$5', SZ}])}.

mmp(O, SX, SY, SZ, LX, LY, LZ) ->
    {#location{x=LX-SX/4, y=LY-SY/4, z=LZ+SZ/4},
     ets:select(O, [{'<', '$3', SX}, {'<', '$4', SY}, {'>=', '$5', SZ}])}.

mpm(O, SX, SY, SZ, LX, LY, LZ) ->
    {#location{x=LX-SX/4, y=LY+SY/4, z=LZ-SZ/4},
     ets:select(O, [{'<', '$3', SX}, {'>=', '$4', SY}, {'<', '$5', SZ}])}.

mpp(O, SX, SY, SZ, LX, LY, LZ) ->
    {#location{x=LX-SX/4, y=LY+SY/4, z=LZ+SZ/4},
     ets:select(O, [{'<', '$3', SX}, {'>=', '$4', SY}, {'>=', '$5', SZ}])}.

pmm(O, SX, SY, SZ, LX, LY, LZ) ->
    {#location{x=LX+SX/4, y=LY-SY/4, z=LZ-SZ/4},
     ets:select(O, [{'>=', '$3', SX}, {'<', '$4', SY}, {'<', '$5', SZ}])}.

pmp(O, SX, SY, SZ, LX, LY, LZ) ->
    {#location{x=LX+SX/4, y=LY-SY/4, z=LZ+SZ/4},
     ets:select(O, [{'>=', '$3', SX}, {'<', '$4', SY}, {'>=', '$5', SZ}])}.

ppm(O, SX, SY, SZ, LX, LY, LZ) ->
    {#location{x=LX+SX/4, y=LY+SY/4, z=LZ-SZ/4},
     ets:select(O, [{'>=', '$3', SX}, {'>=', '$4', SY}, {'<', '$5', SZ}])}.

ppp(O, SX, SY, SZ, LX, LY, LZ) ->
    {#location{x=LX+SX/4, y=LY+SY/4, z=LZ+SZ/4},
     ets:select(O, [{'>=', '$3', SX}, {'>=', '$4', SY}, {'>=', '$5', SZ}])}.

