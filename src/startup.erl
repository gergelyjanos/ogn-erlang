%% @author Janos Gergely <gergelyjanos@td1990.org>
%% @doc Function to startup the OGN converter
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
    Parser = spawn(parser, start, []),
    io:format("Parser Pid ~p~n", [Parser]),

    Client = spawn(aprsclient, start, [Parser]),
    io:format("Client Pid ~p~n", [Client]),

    io:read("running?")
.
