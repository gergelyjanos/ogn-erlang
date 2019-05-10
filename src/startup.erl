%% @author Janos Gergely <gergelyjanos@td1990.org>
%% @doc Function to startup the OGN converter
%% @copyright 2019 TD*1990
%% @version 1.0.0

-module(startup).
-export([main/0]).

main() ->  
    ClientPid = spawn(aprsclient, run, []),
    io:read("running?").
