%% @author Janos Gergely <gergelyjanos@td1990.org>
%% @doc Module to CRUD in DB receiver position data
%% @copyright 2019 TD*1990
%% @version 1.0.0

-module(receiverPositionDb).
-export([start/0, list/0, count/0, receiver/1]).
-include("receiverPositionRecord.hrl").

start() ->
    ets:new(receiverPositionTable, [named_table, {keypos, #receiverPosition.receiver}]),
    run()
.

run() ->
    receive
        {create, Position} ->
            _Inserted = ets:insert(receiverPositionTable, Position),
            ok
    end,
    run()
.

list() ->
    ets:tab2list(receiverPositionTable)
.    

count() ->
    length(ets:tab2list(receiverPositionTable))
.

receiver(Receiver) ->
    ets:lookup(receiverPositionTable, Receiver)
.

