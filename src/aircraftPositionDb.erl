%% @author Janos Gergely <gergelyjanos@td1990.org>
%% @doc Module to CRUD in DB aircraft position data
%% @copyright 2019 TD*1990
%% @version 1.0.0

-module(aircraftPositionDb).
-export([start/0, list/0, count/0]).
-include("aircraftPositionRecord.hrl").

start() ->
    ets:new(aircraftPositionTable, [named_table, {keypos, #aircraftPosition.device}]),
    run()
.

run() ->
    receive
        {create, Position} ->
            _Inserted = ets:insert(aircraftPositionTable, Position),
            ok;
        {update, _Position} ->
            ok;
        {delete, _Position} ->
            ok;
        {readAll, _Filter} ->
            ok
    end,
    run()
.

list() ->
    ets:tab2list(aircraftPositionTable)
.    

count() ->
    length(ets:tab2list(aircraftPositionTable))
.
