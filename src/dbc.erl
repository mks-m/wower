-module(dbc).
-export([read/1, import/1, import_all/0]).

-include_lib("kernel/include/file.hrl").

-record(dbc, {records, fields}).
-define(I, :32/integer-little).

read(File) ->
    FileName = "dbc/" ++ File,
    {ok, FileInfo} = file:read_file_info(FileName),
    {ok, Dbc} = file:open(FileName, [read, raw, binary]),
    {ok, <<(16#43424457)?I>>} = file:read(Dbc, 4),
    {ok, <<Records?I, Fields?I, Size?I, StringSize?I>>} = file:read(Dbc, 16),
    {ok, BinData} = file:read(Dbc, FileInfo#file_info.size - StringSize - 20),
    {ok, StringData} = file:read(Dbc, StringSize).

import(Atom) ->
    ok.

import_all() ->
    ok.
