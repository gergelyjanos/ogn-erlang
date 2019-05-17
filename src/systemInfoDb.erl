%% @author Janos Gergely <gergelyjanos@td1990.org>
%% @doc Module to CRUD in DB system info data
%% @copyright 2019 TD*1990
%% @version 1.0.0

-module(systemInfoDb).
-export([start/0, list/0, count/0, create/2, run/0]).
-include("systemInfoRecord.hrl").
-define(SELF, whereis(systeminfodb)).

start() ->
    ets:new(systemInfoTable, [named_table, {keypos, #systemInfo.id}]),
    SystemInfoDb = spawn(systemInfoDb, run, []),
    io:format("Sysinfo pid ~p~n", [SystemInfoDb]),
    R = register(systeminfodb, SystemInfoDb),
    io:format("Sysinfo pid reg ~p~n", [R]),
    SystemInfoDb
.

run() ->
    receive
        {create, Rec} ->
            _Inserted = ets:insert(systemInfoTable, Rec),
            ok
    end,
    run()
.

create(_MessageType, _Message) ->
    ok
    % Rec = #systemInfo{ 
    %     id = 0, % todo autoinc
    %     createdAt = calendar:universal_time(), 
    %     messageType = MessageType,
    %     message = Message
    % },
    % P = whereis(systeminfodb),
    % io:format("Sysinfo pid ~p~n", [P]),
    % P ! {create, Rec}
.

list() ->
    ets:tab2list(systemInfoTable)
.    

count() ->
    length(ets:tab2list(systemInfoTable))
.
