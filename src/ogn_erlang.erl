%% @author Janos Gergely <gergelyjanos@td1990.org>
%% @doc Module to startup the OGN converter
%% @copyright 2019 TD*1990
%% @version 1.0.0

-module(ogn_erlang).
-behavior(application).
-export([start/2, stop/1, main/0]).

start(_Type, _Args) ->
    main(),
    {ok, self()}.

stop(_State) ->
    ok.

main() ->
    SystemInfoDb = systemInfoDb:spawnandstart(),
    io:format("SystemInfoDb Pid ~p~n", [SystemInfoDb]),

    HttpServer = spawn(httpserver, start, []),
    register(httpserver, HttpServer),
    io:format("HttpServer Pid ~p~n", [HttpServer]),

    AircraftPositionDb = spawn(aircraftPositionDb, start, []),
    register(aircraftPositionDb, AircraftPositionDb),
    io:format("AircraftPositionDb Pid ~p~n", [AircraftPositionDb]),

    ReceiverPositionDb = spawn(receiverPositionDb, start, []),
    register(receiverPositionDb, ReceiverPositionDb),
    io:format("ReceiverPositionDb Pid ~p~n", [ReceiverPositionDb]),

    Parser = spawn(parser, start, []),
    io:format("Parser Pid ~p~n", [Parser]),

    Client = spawn(aprsclient, start, [Parser]),
    io:format("Client Pid ~p~n", [Client]),
    % {ok, null}
    io:read("running?")
.
