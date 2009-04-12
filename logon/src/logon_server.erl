-module(logon_server).
-export([loop/1]).
-export([receiver/2, sender/2]).

-include("logon_records.hrl").
-include("database_records.hrl").

loop(Socket) ->
    R = spawn_link(?MODULE, receiver, [Socket, self()]),
    S = spawn_link(?MODULE, sender, [Socket, self()]),
    client(#logon_state{receiver = R, sender = S}).

client(#logon_state{receiver = R, sender = S} = State) ->
    receive
    {R, Opcode, Data} ->
        Handler = logon_opcodes:get(Opcode),
        case logon_packets:Handler(Opcode, Data, State) of
        {send, Response, NewState} ->
            S ! {self(), Response},
            client(NewState);
        {skip, _, NewState} ->
            client(NewState);
        Any ->
            io:format("unexpected response: ~p~n", Any),
            client(State)
        end;
    Any ->
        io:format("Weird message to client: ~p~n", [Any]),
        client(State)
    end.

receiver(Socket, Client) ->
    case gen_tcp:recv(Socket, 0) of
    {ok, Data} ->
        <<Opcode:8/integer, Rest/binary>> = Data,
        Client ! {self(), Opcode, Rest},
        receiver(Socket, Client);
    {error, closed} -> exit(socket_closed)
    end.

sender(Socket, Client) ->
    receive
        {Client, Data} ->
            gen_tcp:send(Socket, Data),
            sender(Socket, Client);
        Any ->
            io:format("Weird message to sender: ~p~n", [Any]),
            sender(Socket, Client)
    end.
