-module(dbc).
-export([import/1, import_all/0]).

-import(packet_helper, [read_cstring/1]).
-include_lib("kernel/include/file.hrl").
-include("dbc_records.hrl").

-define(I, :32/integer-little).
-define(F, :32/float-little).

import(Atom, File, Struct) ->
    FileName = "dbc/" ++ File ++ ".dbc",
    {ok, FileInfo} = file:read_file_info(FileName),
    {ok, Dbc} = file:open(FileName, [read, raw, binary]),
    {ok, <<(16#43424457)?I>>} = file:read(Dbc, 4),
    {ok, <<Records?I, Fields?I, Size?I, StringSize?I>>} = file:read(Dbc, 16),
    {ok, BinData} = file:read(Dbc, FileInfo#file_info.size - StringSize - 20),
    {ok, StringData} = file:read(Dbc, StringSize),
    read_records(Atom, Records, Struct, Size, BinData, StringData).

read_records(_, 0, _, _, _, _) ->
    ok;
read_records(Atom, RecordsLeft, Struct, Size, BinData, StringData) ->
    {Row, NewBin} = erlang:split_binary(BinData, Size),
    Record = read_record({Atom}, Struct, Row, StringData),
    io:format("~p~n", [Record]),
    mnesia:dirty_write(Atom, Record),
    read_records(Atom, RecordsLeft-1, Struct, Size, NewBin, StringData).

read_record(Element, [], _, _) ->
    Element;
read_record(Element, [{_, Type, Index}|Struct], Bin, String) ->
    Value      = read_field(Type, Index, Bin, String),
    NewElement = erlang:append_element(Element, Value),
    read_record(NewElement, Struct, Bin, String).

read_field(int, Index, Bin, _) ->
    Offset = Index * 4,
    <<_:Offset/binary, Value?I, _/binary>> = Bin,
    Value;
read_field(float, Index, Bin, _) ->
    Offset = Index * 4,
    <<_:Offset/binary, Value?F, _/binary>> = Bin,
    Value;
read_field(cstring, Index, Bin, String) ->
    Offset = Index * 4,
    <<_:Offset/binary, Value?I, _/binary>> = Bin,
    {_, StringStart} = erlang:split_binary(String, Value),
    {StringValue, _} = read_cstring(StringStart),
    StringValue;
read_field(_, _, _, _) ->
    wrong_type.

file_info(dbc_chr_race) ->
    {ok, "ChrRaces", 
     [{id, int, 0}, {faction_template_id, int, 2}, 
      {male_model_id, int, 4}, {female_model_id, int, 5}, 
      {team_id, int, 8}, {cinematic_id, int, 13}, 
      {name, cstring, 14}, {expansion_required, int, 34}]};
file_info(Atom) ->
    {no_file, Atom}.

import(Atom) ->
    {ok, Name, Struct} = file_info(Atom),
    Fields = lists:map(fun({N, _, _}) -> N end, Struct),
    mnesia:delete_table(Atom),
    mnesia:create_table(Atom, [{attributes, Fields}, {disc_copies, [node()]}]),
    ok = import(Atom, Name, Struct),
    ok.

import_all() ->
    mnesia:start(),
    import(dbc_chr_race),
    ok.
