%% Author: kp
%% Created: 25 Тра 2008
%% Description: TODO: Add description to logon_patterns
-module(logon_patterns).
-compile(export_all).

-define(L, 32/unsigned-little-integer).
-define(W, 16/unsigned-little-integer).
-define(B,  8/unsigned-little-integer).
-define(b,   /bytes).

auth_request(<<Cmd:?B, Err:?B, Size:?W, Game:4?b, 
               Major:?B, Middle:?B, Minor:?B, Build:?W, 
               Platform:4?b, Os:4?b, Country:4?b,
               TimeZone:?L, IP:4?b, Length:?B, Account:Length?b>>) ->
    io:format("auth request found~n", []),
    {ok, auth_request, Account};
%%    {ok, auth_request, {Cmd, Err, Size, Game, Major, Middle, Minor, Build, 
%%                        Platform, Os, Country, TimeZone, IP, Account}};
auth_request(_) -> 
    io:format("auth request fails~n", []),
    fail.
