%% @author Janos Gergely <gergelyjanos@td1990.org>
%% @doc Function to startup the OGN converter
%% @copyright 2019 TD*1990
%% @version 1.0.0

-module(startup).
-export([main/0]).

main() ->
    Parser = spawn(parser, run, []),
    io:format("Parser Pid ~p~n", [Parser]),
    Client = spawn(aprsclient, run, [Parser]),
    io:format("Client Pid ~p~n", [Client]),
    io:read("running?")
.
