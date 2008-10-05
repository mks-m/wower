-module(char_helper).
-compile(export_all).

equipment(_) ->
    lists:seq(1,20).

gender(male)   -> 0;
gender(female) -> 1;
gender(none)   -> 2.

race(human)     -> 1;
race(orc)       -> 2;
race(dwarf)     -> 3;
race(night_elf) -> 4;
race(undead)    -> 5;
race(tauren)    -> 6;
race(gnome)     -> 7;
race(troll)     -> 8;
race(bloodelf)  -> 10;
race(draenei)   -> 11.

class(warrior)      -> 1;
class(paladin)      -> 2;
class(hunter)       -> 3;
class(rogue)        -> 4;
class(priest)       -> 5;
class(death_knight) -> 6;
class(shaman)       -> 7;
class(mage)         -> 8;
class(warlock)      -> 9;
class(druid)        -> 11.

reputation(hated)      -> 0;
reputation(hostile)    -> 1;
reputation(unfriendly) -> 2;
reputation(neutral)    -> 3;
reputation(friendly)   -> 4;
reputation(honored)    -> 5;
reputation(revered)    -> 6;
reputation(exalted)    -> 7.

money(copper) -> 1;
money(silver) -> 100;
money(gold)   -> 10000.

stat(strength)  -> 0;
stat(agility)   -> 1;
stat(stamina)   -> 2;
stat(intellect) -> 3;
stat(spirit)    -> 4.

power(mana)        -> 0;
power(rage)        -> 1;
power(focus)       -> 2;
power(energy)      -> 3;
power(happiness)   -> 4;
power(rune)        -> 5;
power(runic_power) -> 6;
power(health)      -> -2.
