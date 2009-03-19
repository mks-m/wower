-module(content).
-export([char_create_info/2]).

-include("more_records.hrl").

char_create_info(Race, Class) -> 
    CI1 = race_create_info(Race, #char_create_info{}),
    CI2 = class_create_info(Class, CI1),
    race_class_create_info(Race, Class, CI2).

race_class_create_info(human,     warrior, Rec) -> Rec#char_create_info{health = 60, power = 100};
race_class_create_info(orc,       warrior, Rec) -> Rec#char_create_info{health = 80, power = 100};
race_class_create_info(dwarf,     warrior, Rec) -> Rec#char_create_info{health = 90, power = 100};
race_class_create_info(night_elf, warrior, Rec) -> Rec#char_create_info{health = 50, power = 100};
race_class_create_info(undead,    warrior, Rec) -> Rec#char_create_info{health = 70, power = 100};
race_class_create_info(tauren,    warrior, Rec) -> Rec#char_create_info{health = 80, power = 100};
race_class_create_info(gnome,     warrior, Rec) -> Rec#char_create_info{health = 50, power = 100};
race_class_create_info(troll,     warrior, Rec) -> Rec#char_create_info{health = 70, power = 100};
race_class_create_info(blood_elf, warrior, Rec) -> Rec#char_create_info{health = 50, power = 100};
race_class_create_info(draenei,   warrior, Rec) -> Rec#char_create_info{health = 70, power = 100};

race_class_create_info(human,     paladin, Rec) -> Rec#char_create_info{health = 58, power = 80};
race_class_create_info(dwarf,     paladin, Rec) -> Rec#char_create_info{health = 88, power = 79};
race_class_create_info(blood_elf, paladin, Rec) -> Rec#char_create_info{health = 38, power = 140};
race_class_create_info(draenei,   paladin, Rec) -> Rec#char_create_info{health = 48, power = 95};

race_class_create_info(orc,       hunter, Rec) -> Rec#char_create_info{health = 76, power = 82};
race_class_create_info(dwarf,     hunter, Rec) -> Rec#char_create_info{health = 86, power = 84};
race_class_create_info(night_elf, hunter, Rec) -> Rec#char_create_info{health = 46, power = 85};
race_class_create_info(tauren,    hunter, Rec) -> Rec#char_create_info{health = 76, power = 80};
race_class_create_info(troll,     hunter, Rec) -> Rec#char_create_info{health = 66, power = 81};
race_class_create_info(blood_elf, hunter, Rec) -> Rec#char_create_info{health = 45, power = 145};
race_class_create_info(draenei,   hunter, Rec) -> Rec#char_create_info{health = 46, power = 100};

race_class_create_info(human,     rogue, Rec) -> Rec#char_create_info{health = 55, power = 100};
race_class_create_info(orc,       rogue, Rec) -> Rec#char_create_info{health = 75, power = 100};
race_class_create_info(dwarf,     rogue, Rec) -> Rec#char_create_info{health = 85, power = 100};
race_class_create_info(night_elf, rogue, Rec) -> Rec#char_create_info{health = 45, power = 100};
race_class_create_info(undead,    rogue, Rec) -> Rec#char_create_info{health = 65, power = 100};
race_class_create_info(gnome,     rogue, Rec) -> Rec#char_create_info{health = 45, power = 100};
race_class_create_info(troll,     rogue, Rec) -> Rec#char_create_info{health = 65, power = 100};
race_class_create_info(blood_elf, rogue, Rec) -> Rec#char_create_info{health = 44, power = 100};

race_class_create_info(human,     priest, Rec) -> Rec#char_create_info{health = 52, power = 160};
race_class_create_info(dwarf,     priest, Rec) -> Rec#char_create_info{health = 82, power = 145};
race_class_create_info(night_elf, priest, Rec) -> Rec#char_create_info{health = 51, power = 160};
race_class_create_info(undead,    priest, Rec) -> Rec#char_create_info{health = 62, power = 130};
race_class_create_info(troll,     priest, Rec) -> Rec#char_create_info{health = 62, power = 128};
race_class_create_info(blood_elf, priest, Rec) -> Rec#char_create_info{health = 50, power = 220};
race_class_create_info(draenei,   priest, Rec) -> Rec#char_create_info{health = 51, power = 175};

% TODO: find death knight start values
race_class_create_info(human,     death_knight, Rec) -> Rec#char_create_info{health = 60, power = 1000};
race_class_create_info(orc,       death_knight, Rec) -> Rec#char_create_info{health = 60, power = 1000};
race_class_create_info(dwarf,     death_knight, Rec) -> Rec#char_create_info{health = 60, power = 1000};
race_class_create_info(night_elf, death_knight, Rec) -> Rec#char_create_info{health = 60, power = 1000};
race_class_create_info(undead,    death_knight, Rec) -> Rec#char_create_info{health = 60, power = 1000};
race_class_create_info(tauren,    death_knight, Rec) -> Rec#char_create_info{health = 60, power = 1000};
race_class_create_info(gnome,     death_knight, Rec) -> Rec#char_create_info{health = 60, power = 1000};
race_class_create_info(troll,     death_knight, Rec) -> Rec#char_create_info{health = 60, power = 1000};
race_class_create_info(blood_elf, death_knight, Rec) -> Rec#char_create_info{health = 60, power = 1000};
race_class_create_info(draenei,   death_knight, Rec) -> Rec#char_create_info{health = 60, power = 1000};

race_class_create_info(orc,       shaman, Rec) -> Rec#char_create_info{health = 77, power = 73};
race_class_create_info(tauren,    shaman, Rec) -> Rec#char_create_info{health = 77, power = 71};
race_class_create_info(troll,     shaman, Rec) -> Rec#char_create_info{health = 67, power = 72};
race_class_create_info(draenei,   shaman, Rec) -> Rec#char_create_info{health = 47, power = 105};

race_class_create_info(human,     mage, Rec) -> Rec#char_create_info{health = 52, power = 165};
race_class_create_info(undead,    mage, Rec) -> Rec#char_create_info{health = 62, power = 135};
race_class_create_info(gnome,     mage, Rec) -> Rec#char_create_info{health = 51, power = 210};
race_class_create_info(troll,     mage, Rec) -> Rec#char_create_info{health = 62, power = 119};
race_class_create_info(blood_elf, mage, Rec) -> Rec#char_create_info{health = 50, power = 225};
race_class_create_info(draenei,   mage, Rec) -> Rec#char_create_info{health = 51, power = 180};

race_class_create_info(human,     warlock, Rec) -> Rec#char_create_info{health = 53, power = 140};
race_class_create_info(orc,       warlock, Rec) -> Rec#char_create_info{health = 73, power = 109};
race_class_create_info(undead,    warlock, Rec) -> Rec#char_create_info{health = 63, power = 110};
race_class_create_info(gnome,     warlock, Rec) -> Rec#char_create_info{health = 43, power = 185};
race_class_create_info(blood_elf, warlock, Rec) -> Rec#char_create_info{health = 42, power = 200};

race_class_create_info(night_elf, druid, Rec) -> Rec#char_create_info{health = 53, power = 100};
race_class_create_info(tauren,    druid, Rec) -> Rec#char_create_info{health = 74, power = 67};
race_class_create_info(_, _, _) ->
    wrong_race_class.

race_create_info(human, Rec) ->
    Rec#char_create_info{faction_template = 1, 
                         display_id       = 49, 
                         scale            = 1, 
                         map_id           = 0, 
                         zone_id          = 12, 
                         intro            = 81,
                         position_x       = -8949.95, 
                         position_y       = -132.493, 
                         position_z       = 83.5312, 
                         orientation      = 0,
                         agility          = 20, 
                         intellect        = 20, 
                         spirit           = 20, 
                         stamina          = 20, 
                         strength         = 20};
race_create_info(orc, Rec) ->
    Rec#char_create_info{faction_template = 2, 
                         display_id       = 51, 
                         scale            = 1, 
                         map_id           = 1, 
                         zone_id          = 14, 
                         intro            = 21,
                         position_x       = -618.518, 
                         position_y       = -4251.67, 
                         position_z       = 38.718, 
                         orientation      = 0,
                         agility          = 17, 
                         intellect        = 17, 
                         spirit           = 23, 
                         stamina          = 22, 
                         strength         = 23};
race_create_info(dwarf, Rec) ->
    Rec#char_create_info{faction_template = 3, 
                         display_id       = 53, 
                         scale            = 1, 
                         map_id           = 0, 
                         zone_id          = 1, 
                         intro            = 41,
                         position_x       = -6240.32,
                         position_y       = 331.033,
                         position_z       = 382.758, 
                         orientation      = 0,
                         agility          = 16, 
                         intellect        = 19, 
                         spirit           = 19, 
                         stamina          = 23, 
                         strength         = 22};
race_create_info(night_elf, Rec) ->
    Rec#char_create_info{faction_template = 4, 
                         display_id       = 55, 
                         scale            = 1, 
                         map_id           = 1, 
                         zone_id          = 141, 
                         intro            = 61,
                         position_x       = 10311.3, 
                         position_y       = 832.463, 
                         position_z       = 1326.41, 
                         orientation      = 0,
                         agility          = 25, 
                         intellect        = 20, 
                         spirit           = 20, 
                         stamina          = 19, 
                         strength         = 17};
race_create_info(undead, Rec) ->
    Rec#char_create_info{faction_template = 5, 
                         display_id       = 57, 
                         scale            = 1, 
                         map_id           = 0, 
                         zone_id          = 85, 
                         intro            = 2,
                         position_x       = 1676.35, 
                         position_y       = 1677.45, 
                         position_z       = 121.67, 
                         orientation      = 0,
                         agility          = 18, 
                         intellect        = 18, 
                         spirit           = 25, 
                         stamina          = 21, 
                         strength         = 19};
race_create_info(tauren, Rec) ->
    Rec#char_create_info{faction_template = 6, 
                         display_id       = 59, 
                         scale            = 1.3, 
                         map_id           = 1, 
                         zone_id          = 215, 
                         intro            = 141,
                         position_x       = -2917.58, 
                         position_y       = -257.98, 
                         position_z       = 52.9968, 
                         orientation      = 0,
                         agility          = 15, 
                         intellect        = 15, 
                         spirit           = 22, 
                         stamina          = 22, 
                         strength         = 25};
race_create_info(gnome, Rec) ->
    Rec#char_create_info{faction_template = 8, 
                         display_id       = 1563, 
                         scale            = 1, 
                         map_id           = 0, 
                         zone_id          = 1, 
                         intro            = 101,
                         position_x       = -6237.02, 
                         position_y       = 329.659, 
                         position_z       = 382.703, 
                         orientation      = 0,
                         agility          = 23, 
                         intellect        = 23, 
                         spirit           = 20, 
                         stamina          = 19, 
                         strength         = 15};
race_create_info(troll, Rec) ->
    Rec#char_create_info{faction_template = 9, 
                         display_id       = 1478, 
                         scale            = 1, 
                         map_id           = 1, 
                         zone_id          = 14, 
                         intro            = 121,
                         position_x       = -618.518, 
                         position_y       = -4251.67, 
                         position_z       = 38.718, 
                         orientation      = 0,
                         agility          = 22, 
                         intellect        = 16, 
                         spirit           = 21, 
                         stamina          = 21, 
                         strength         = 21};
race_create_info(blood_elf, Rec) ->
    Rec#char_create_info{faction_template = 914, 
                         display_id       = 15467, 
                         scale            = 1, 
                         map_id           = 530, 
                         zone_id          = 3430, 
                         intro            = 162,
                         position_x       = 10349.6, 
                         position_y       = -6357.29, 
                         position_z       = 33.4026, 
                         orientation      = 0,
                         agility          = 22, 
                         intellect        = 24, 
                         spirit           = 19, 
                         stamina          = 18, 
                         strength         = 17};
race_create_info(draenei, Rec) ->
    Rec#char_create_info{faction_template = 927, 
                         display_id       = 16125, 
                         scale            = 1, 
                         map_id           = 530, 
                         zone_id          = 3524, 
                         intro            = 163,
                         position_x       = -3961.64, 
                         position_y       = -13931.2, 
                         position_z       = 100.615, 
                         orientation      = 0,
                         agility          = 17, 
                         intellect        = 21, 
                         spirit           = 22, 
                         stamina          = 19, 
                         strength         = 21};
race_create_info(_, _) ->
    wrong_race.

class_create_info(warrior, Rec) ->
    Rec#char_create_info{agility    = 0.81 * Rec#char_create_info.agility, 
                         intellect  = 0.2  * Rec#char_create_info.intellect,
                         spirit     = 0.3  * Rec#char_create_info.spirit,
                         stamina    = 1.1  * Rec#char_create_info.stamina, 
                         strength   = 1.2  * Rec#char_create_info.strength, 
                         min_dmg    = 5,
                         max_dmg    = 7,
                         power_type = rage};
class_create_info(paladin, Rec) ->
    Rec#char_create_info{agility    = 0.6  * Rec#char_create_info.agility, 
                         intellect  = 0.7  * Rec#char_create_info.intellect,
                         spirit     = 0.77 * Rec#char_create_info.spirit,
                         stamina    = 1.2  * Rec#char_create_info.stamina, 
                         strength   = 1.1  * Rec#char_create_info.strength,
                         min_dmg    = 6,
                         max_dmg    = 7,
                         power_type = mana};
class_create_info(hunter, Rec) ->
    Rec#char_create_info{agility    = 1.3  * Rec#char_create_info.agility, 
                         intellect  = 0.6  * Rec#char_create_info.intellect,
                         spirit     = 0.66 * Rec#char_create_info.spirit,
                         stamina    = 0.89  * Rec#char_create_info.stamina, 
                         strength   = 0.49  * Rec#char_create_info.strength,
                         min_dmg    = 5,
                         max_dmg    = 7,
                         power_type = mana};
class_create_info(rogue, Rec) ->
    Rec#char_create_info{agility    = 1.5  * Rec#char_create_info.agility, 
                         intellect  = 0.22 * Rec#char_create_info.intellect,
                         spirit     = 0.42 * Rec#char_create_info.spirit,
                         stamina    = 0.78 * Rec#char_create_info.stamina, 
                         strength   = 0.83 * Rec#char_create_info.strength,
                         min_dmg    = 6,
                         max_dmg    = 7,
                         power_type = energy};
class_create_info(priest, Rec) ->
    Rec#char_create_info{agility    = 0.3  * Rec#char_create_info.agility, 
                         intellect  = 1.3  * Rec#char_create_info.intellect,
                         spirit     = 1.39 * Rec#char_create_info.spirit,
                         stamina    = 0.4  * Rec#char_create_info.stamina, 
                         strength   = 0.3  * Rec#char_create_info.strength,
                         min_dmg    = 2,
                         max_dmg    = 4,
                         power_type = mana};
class_create_info(shaman, Rec) ->
    Rec#char_create_info{agility    = 0.5  * Rec#char_create_info.agility, 
                         intellect  = 0.9  * Rec#char_create_info.intellect,
                         spirit     = 1    * Rec#char_create_info.spirit,
                         stamina    = 0.9  * Rec#char_create_info.stamina, 
                         strength   = 0.81 * Rec#char_create_info.strength,
                         min_dmg    = 2,
                         max_dmg    = 4,
                         power_type = mana};
class_create_info(mage, Rec) ->
    Rec#char_create_info{agility    = 0.2  * Rec#char_create_info.agility, 
                         intellect  = 1.44 * Rec#char_create_info.intellect,
                         spirit     = 1.33 * Rec#char_create_info.spirit,
                         stamina    = 0.33 * Rec#char_create_info.stamina, 
                         strength   = 0.14 * Rec#char_create_info.strength,
                         min_dmg    = 2,
                         max_dmg    = 4,
                         power_type = mana};
class_create_info(warlock, Rec) ->
    Rec#char_create_info{agility    = 0.4  * Rec#char_create_info.agility, 
                         intellect  = 1.2  * Rec#char_create_info.intellect,
                         spirit     = 0.7  * Rec#char_create_info.spirit,
                         stamina    = 0.7  * Rec#char_create_info.stamina, 
                         strength   = 0.37 * Rec#char_create_info.strength,
                         min_dmg    = 3.28,
                         max_dmg    = 5.28,
                         power_type = mana};
class_create_info(druid, Rec) ->
    Rec#char_create_info{agility    = 0.5  * Rec#char_create_info.agility, 
                         intellect  = 1    * Rec#char_create_info.intellect,
                         spirit     = 1.1  * Rec#char_create_info.spirit,
                         stamina    = 0.66 * Rec#char_create_info.stamina, 
                         strength   = 0.6  * Rec#char_create_info.strength,
                         min_dmg    = 2,
                         max_dmg    = 4,
                         power_type = mana};
class_create_info(_, _) ->
    wrong_class.
