-module(dbc).
-export([import/1, import_all/0]).

-include_lib("kernel/include/file.hrl").

-define(I, :32/integer-little).
-define(F, :32/float-little).

import(Atom, File, Struct) ->
    FileName = "dbc/" ++ File ++ ".dbc",
    {ok, FileInfo} = file:read_file_info(FileName),
    {ok, Dbc} = file:open(FileName, [read, raw, binary]),
    {ok, <<(16#43424457)?I>>} = file:read(Dbc, 4),
    {ok, <<Records?I, _?I, Size?I, StringSize?I>>} = file:read(Dbc, 16),
    {ok, BinData} = file:read(Dbc, FileInfo#file_info.size - StringSize - 20),
    {ok, StringData} = file:read(Dbc, StringSize),
    read_records(Atom, Records, Struct, Size, BinData, StringData).

read_records(_, 0, _, _, _, _) ->
    ok;
read_records(Atom, RecordsLeft, Struct, Size, BinData, StringData) ->
    {Row, NewBin} = erlang:split_binary(BinData, Size),
    mnesia:write(read_record({Atom}, Struct, Row, StringData)),
    read_records(Atom, RecordsLeft-1, Struct, Size, NewBin, StringData).

read_record(Element, [], _, _) ->
    Element;
read_record(Element, [{_, Type, Index}|Struct], Bin, String) ->
    {Value, NewBin} = read_field(Type, Index, Bin),
    NewElement = erlang:append_element(Element, Value),
    read_record(NewElement, Struct, NewBin, String).

read_field(int, Index, Bin) ->
    <<_:Index/binary, Value?I, Rest>> = Bin,
    {Value, Rest};
read_field(float, Index, Bin) ->
    <<_:Index/binary, Value?I, Rest>> = Bin,
    {Value, Rest};
read_field(cstring, Index, Bin) ->
    <<_:Index/binary, Value?I, Rest>> = Bin,
    {Value, Rest};
read_field(_, _, _) ->
    wrong_type.

file_info(chr_race) ->
    {ok, "ChrRaces", 
     [{id, int, 0}, {faction_template_id, int, 2}, 
      {male_model_id, int, 4}, {female_model_id, int, 5}, 
      {team_id, int, 8}, {cinematic_id, int, 13}, 
      {name, cstring, 14}, {expansion_required, int, 34}]};
file_info(Atom) ->
    {no_file, Atom}.

import(Atom) ->
    {ok, Name, Struct} = file_info(Atom),
    mnesia:delete_table(Atom),
    mnesia:create_table(Atom, [{access_mode, read_only}]),
    ok = import(Atom, Name, Struct),
    ok.

import_all() ->
    import(chr_races),
    ok.
