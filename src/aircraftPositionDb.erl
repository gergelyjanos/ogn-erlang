%% @author Janos Gergely <gergelyjanos@td1990.org>
%% @doc Function to CRUD in DB aircraft position data
%% @copyright 2019 TD*1990
%% @version 1.0.0

-module(aircraftPositionDb).
-export([start/0]).
-include("aircraftPositionRecord.hrl").

start() ->
    run()
.

run() ->
    receive
        {create, position} ->
            ok;
        {update, position} ->
            ok;
        {delete, position} ->
            ok;
        {readAll, filter} ->
            ok
    end,
    run()
.

