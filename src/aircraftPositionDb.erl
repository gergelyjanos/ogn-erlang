%% @author Janos Gergely <gergelyjanos@td1990.org>
%% @doc Module to CRUD in DB aircraft position data
%% @copyright 2019 TD*1990
%% @version 1.0.0

-module(aircraftPositionDb).
-export([start/0, list/0]).
-include("aircraftPositionRecord.hrl").

start() ->
    Table = ets:new(aircraftPositionTable, [named_table, {keypos, #aircraftPosition.device}]),
    run(Table)
.

run(Table) ->
    receive
        {create, Position} ->
            _Inserted = ets:insert(Table, Position),
            % io:format("Aircraft Db created ~p ~p~n", [Inserted, Position]),
            ok;
        {update, _Position} ->
            ok;
        {delete, _Position} ->
            ok;
        {readAll, _Filter} ->
            ok
    end,
    run(Table)
.

list() ->
    [count, ets:select_count(aircraftPositionTable, [])]
.    
