% TODO: this will be oct-tree based process 
%       pool for handling objects in region
-module(cell).
-export([create/1, create/3,
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

create(#size{x=X, y=Y, z=Z} = Size) ->
    Info = #info{s=Size, l=#location{x=X/2, y=Y/2, z=Z/2}},
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
    {bc, From, Range, Message} ->
        % TODO: implement broadcasting to objects
        cell(Info, Objects)
    end.

% TODO: merge meta-cell into one cell
meta(Info, Objects) ->
    receive
    _ ->
        meta(Info, Objects)
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
    meta(Info#info{n=Navigation}, Objects).

mmm(O, SX, SY, SZ, LX, LY, LZ) ->
    NL = #location{x=LX-SX/4, y=LY-SY/4, z=LZ-SZ/4},
    NO = ets:select(O, [{'<', '$3', SX}, {'<', '$4', SY}, {'<', '$5', SZ}]),
    {NL, NO}.

mmp(O, SX, SY, SZ, LX, LY, LZ) ->
    NL = #location{x=LX-SX/4, y=LY-SY/4, z=LZ+SZ/4},
    NO = ets:select(O, [{'<', '$3', SX}, {'<', '$4', SY}, {'>=', '$5', SZ}]),
    {NL, NO}.

mpm(O, SX, SY, SZ, LX, LY, LZ) ->
    NL = #location{x=LX-SX/4, y=LY+SY/4, z=LZ-SZ/4},
    NO = ets:select(O, [{'<', '$3', SX}, {'>=', '$4', SY}, {'<', '$5', SZ}]),
    {NL, NO}.

mpp(O, SX, SY, SZ, LX, LY, LZ) ->
    NL = #location{x=LX-SX/4, y=LY+SY/4, z=LZ+SZ/4},
    NO = ets:select(O, [{'<', '$3', SX}, {'>=', '$4', SY}, {'>=', '$5', SZ}]),
    {NL, NO}.

pmm(O, SX, SY, SZ, LX, LY, LZ) ->
    NL = #location{x=LX+SX/4, y=LY-SY/4, z=LZ-SZ/4},
    NO = ets:select(O, [{'>=', '$3', SX}, {'<', '$4', SY}, {'<', '$5', SZ}]),
    {NL, NO}.

pmp(O, SX, SY, SZ, LX, LY, LZ) ->
    NL = #location{x=LX+SX/4, y=LY-SY/4, z=LZ+SZ/4},
    NO = ets:select(O, [{'>=', '$3', SX}, {'<', '$4', SY}, {'>=', '$5', SZ}]),
    {NL, NO}.

ppm(O, SX, SY, SZ, LX, LY, LZ) ->
    NL = #location{x=LX+SX/4, y=LY+SY/4, z=LZ-SZ/4},
    NO = ets:select(O, [{'>=', '$3', SX}, {'>=', '$4', SY}, {'<', '$5', SZ}]),
    {NL, NO}.

ppp(O, SX, SY, SZ, LX, LY, LZ) ->
    NL = #location{x=LX+SX/4, y=LY+SY/4, z=LZ+SZ/4},
    NO = ets:select(O, [{'>=', '$3', SX}, {'>=', '$4', SY}, {'>=', '$5', SZ}]),
    {NL, NO}.

