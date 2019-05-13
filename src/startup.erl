%% @author Janos Gergely <gergelyjanos@td1990.org>
%% @doc Module to startup the OGN converter
%% @copyright 2019 TD*1990
%% @version 1.0.0

-module(startup).
-behavior(application).
-export([start/2, stop/1, main/0]).

start(_Type, _Args) ->
    main().

stop(_State) ->
    ok.

main() ->
    HttpServer = spawn(httpserver, start, []),
    register(httpserver, HttpServer),
    io:format("Parser Pid ~p~n", [HttpServer]),

    AircraftPositionDb = spawn(aircraftPositionDb, start, []),
    register(aircraftPositionDb, AircraftPositionDb),
    io:format("Parser Pid ~p~n", [AircraftPositionDb]),

    Parser = spawn(parser, start, []),
    io:format("Parser Pid ~p~n", [Parser]),

    Client = spawn(aprsclient, start, [Parser]),
    io:format("Client Pid ~p~n", [Client]),

    io:read("running?")
.
