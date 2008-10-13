% TODO: this will be oct-tree based process 
%       pool for handling objects in region
-module(cell).
-export([create/3, init/3]).

create(M, F, A) ->
    spawn_link(?MODULE, init, [M, F, A]).

init(M, F, A) ->
    Pid = spawn_link(M, F, [self()|A]),
    cell(Pid).

cell(Pid) ->
    receive
    {Pid, split, A} ->
        Cells = collect(A),
        Pid ! {self(), split, Cells},
        meta(Cells);
    Message ->
        Pid ! Message,
        cell(Pid)
    end.

% TODO: merge meta-cell into one cell
meta(Cells) ->
    receive
    Message ->
        spread(Cells, Message),
        meta(Cells)
    end.

spread([], _) ->
    ok;
spread([Cell|Cells], Message) ->
    Cell ! Message,
    spread(Cells, Message).

collect(A) ->
    collect(A, []).

collect([], Cells) ->
    Cells;
collect([{M, F, A}|Rest], Cells) ->
    Pid = create(M, F, A),
    collect(Rest, [Pid|Cells]).
