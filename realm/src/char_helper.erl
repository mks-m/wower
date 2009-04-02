-module(char_helper).
-compile(export_all).
-import(common_helper, [do/1]).

-include("database_records.hrl").
-include_lib("stdlib/include/qlc.hrl").

%% @spec find(int()) -> tuple().
find(Id) ->
    [Char] = do(qlc:q([X || X <- mnesia:table(char), X#char.id =:= Id])),
    Char.

%% @spec equipment(any()) -> list().
equipment(_) ->
    lists:seq(1,20).

%% @spec unit_bytes_0(tuple()) -> binary().
unit_bytes_0(#char{race = R, class = C, gender = G, power_type = P}) ->
    <<UB:32/integer-little>> = <<(race(R)):8, (class(C)):8, (gender(G)):8, (power(P)):8>>,
    UB.

%% @spec player_bytes(tuple()) -> binary().
player_bytes(#char{skin = S, face = F, hair_style = Hs, hair_color = Hc}) ->
    <<PB:32/integer-little>> = <<S:8, F:8, Hs:8, Hc:8>>,
    PB.

%% @spec player_bytes_2(tuple()) -> binary().
player_bytes_2(#char{facial_hair = F}) ->
    <<PB:32/integer-little>> = <<F:8, 238:8, 0:8, 2:8>>,
    PB.

%% @type gender() = male | female | none.
%% @spec gender(gender()) -> int().
%% @spec gender(int()) -> gender().
gender(male)   -> 0; gender(0) -> male;
gender(female) -> 1; gender(1) -> female;
gender(none)   -> 2; gender(2) -> none.

%% @type race() = human | orc | dwarf | night_elf | 
%%                undead | tauren | gnome | troll |
%%                blood_elf | draenei.
%% @spec race(race()) -> int().
%% @spec race(int()) -> race().
race(human)     -> 1;  race(1)  -> human;
race(orc)       -> 2;  race(2)  -> orc;
race(dwarf)     -> 3;  race(3)  -> dwarf;
race(night_elf) -> 4;  race(4)  -> night_elf;
race(undead)    -> 5;  race(5)  -> undead;
race(tauren)    -> 6;  race(6)  -> tauren;
race(gnome)     -> 7;  race(7)  -> gnome;
race(troll)     -> 8;  race(8)  -> troll;
race(blood_elf) -> 10; race(10) -> blood_elf;
race(draenei)   -> 11; race(11) -> draenei.

%% @type class() = warrior | paladin | hunter | rogue |
%%                 priest | death_knight | shaman |
%%                 mage | warlock | druid.
%% @spec class(class()) -> int().
%% @spec class(int()) -> class().
class(warrior)      -> 1;  class(1)  -> warrior;
class(paladin)      -> 2;  class(2)  -> paladin;
class(hunter)       -> 3;  class(3)  -> hunter;
class(rogue)        -> 4;  class(4)  -> rogue;
class(priest)       -> 5;  class(5)  -> priest;
class(death_knight) -> 6;  class(6)  -> death_knight;
class(shaman)       -> 7;  class(7)  -> shaman;
class(mage)         -> 8;  class(8)  -> mage;
class(warlock)      -> 9;  class(9)  -> warlock;
class(druid)        -> 11; class(11) -> druid.

%% @type reputation() = hated | hostile | unfriendly |
%%                      neutral | friendly | honored |
%%                      revered | exalted.
%% @spec reputation(reputation()) -> int().
reputation(hated)      -> 0;
reputation(hostile)    -> 1;
reputation(unfriendly) -> 2;
reputation(neutral)    -> 3;
reputation(friendly)   -> 4;
reputation(honored)    -> 5;
reputation(revered)    -> 6;
reputation(exalted)    -> 7.


%% @type money() = copper | silver | gold.
%% @spec money(money()) -> int().
money(copper) -> 1;
money(silver) -> 100;
money(gold)   -> 10000.

%% @type stat() = strength | agility | stamina | intellect | spirit.
%% @spec stat(stat()) -> int().
stat(strength)  -> 0;
stat(agility)   -> 1;
stat(stamina)   -> 2;
stat(intellect) -> 3;
stat(spirit)    -> 4.

%% @type power() = mana | rage | focus | energy |
%%                 happiness | rune | runic_power | health.
%% @spec power(power()) -> int().
power(mana)        -> 0;
power(rage)        -> 1;
power(focus)       -> 2;
power(energy)      -> 3;
power(happiness)   -> 4;
power(rune)        -> 5;
power(runic_power) -> 6;
power(health)      -> -2.
