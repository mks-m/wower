-module(content).
-compile(export_all).

-include("more_records.hrl").

race_create_info(human, Rec) ->
    Rec#char_create_info{faction_template = 1,
                         display_id       = 49,
                         scale            = 1,
                         map_id           = 0,
                         zone_id          = 12,
                         position_x       = -8949.95,
                         position_y       = -132.493,
                         position_z       = 83.5312,
                         orientation      = 0,
                         agility          = 20,
                         intellect        = 20,
                         spirit           = 20,
                         stamina          = 20,
                         strength         = 20}.

class_create_info(warrior, Rec) ->
    Rec#char_create_info{agility    = 0.81 * Rec#char_create_info.agility, 
                         intellect  = 0.2  * Rec#char_create_info.intellect,
                         spirit     = 0.3  * Rec#char_create_info.spirit,
                         stamina    = 1.1  * Rec#char_create_info.stamina, 
                         strength   = 1.2  * Rec#char_create_info.strength, 
                         min_dmg    = 5,
                         max_dmg    = 7,
                         power_type = rage}.

char_create_info(human, warrior) -> 
    CI1 = race_create_info(human, #char_create_info{}),
    CI2 = class_create_info(warrior, CI1),
    CI2#char_create_info{health = 60, power  = 100};
char_create_info(human, paladin) -> ok;
char_create_info(human, rogue) -> ok;
char_create_info(human, priest) -> ok;
char_create_info(human, death_knight) -> ok;
char_create_info(human, mage) -> ok;
char_create_info(human, warlock) -> ok;

char_create_info(dwarf, warrior) -> ok;
char_create_info(dwarf, paladin) -> ok;
char_create_info(dwarf, hunter) -> ok;
char_create_info(dwarf, rogue) -> ok;
char_create_info(dwarf, priest) -> ok;
char_create_info(dwarf, death_knight) -> ok;

char_create_info(night_elf, warrior) -> ok;
char_create_info(night_elf, hunter) -> ok;
char_create_info(night_elf, rogue) -> ok;
char_create_info(night_elf, priest) -> ok;
char_create_info(night_elf, death_knight) -> ok;
char_create_info(night_elf, druid) -> ok;

char_create_info(gnome, warrior) -> ok;
char_create_info(gnome, rogue) -> ok;
char_create_info(gnome, mage) -> ok;
char_create_info(gnome, warlock) -> ok;
char_create_info(gnome, death_knight) -> ok;
char_create_info(_, _) -> unknown.
