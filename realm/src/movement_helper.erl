-module(movement_helper).
-compile(export_all).

flag(forward) ->      16#00000001;
flag(backward) ->     16#00000002;
flag(strafe_left) ->  16#00000004;
flag(strafe_right) -> 16#00000008;
flag(left) ->         16#00000010;
flag(right) ->        16#00000020;
flag(pitch_up) ->     16#00000040;
flag(pitch_down) ->   16#00000080;
flag(walk_mode) ->    16#00000100;
flag(on_transport) -> 16#00000200;
flag(levitating) ->   16#00000400;
flag(fly_unk1) ->     16#00000800;
flag(jumping) ->      16#00001000;
flag(unk4) ->         16#00002000;
flag(falling) ->      16#00004000;
flag(swimming) ->     16#00200000;
flag(fly_up) ->       16#00400000;
flag(can_fly) ->      16#00800000;
flag(flying) ->       16#01000000;
flag(flying2) ->      16#02000000;
flag(spline) ->       16#04000000;
flag(spline2) ->      16#08000000;
flag(waterwalking) -> 16#10000000;
flag(safe_fall) ->    16#20000000;
flag(unk3) ->         16#40000000.