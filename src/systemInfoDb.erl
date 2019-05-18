%% @author Janos Gergely <gergelyjanos@td1990.org>
%% @doc Module to CRUD in DB system info data
%% @copyright 2019 TD*1990
%% @version 1.0.0

-module(systemInfoDb).
-export([start/0, list/0, count/0, create/3, spawnandstart/0]).
-include("systemInfoRecord.hrl").
-define(SELF, whereis(systeminfodb)).

spawnandstart() ->
    SystemInfoDb = spawn(systemInfoDb, start, []),
    io:format("Sysinfo pid ~p~n", [SystemInfoDb]),
    R = register(systeminfodb, SystemInfoDb),
    io:format("Sysinfo pid reg ~p~n", [R]),
    create(dbstarted, [], "DB started"),
    SystemInfoDb
.

start() ->
    ets:new(systemInfoTable, [named_table, {keypos, #systemInfo.infoType}]),
    run()
.

run() ->
    receive
        {create, Rec} ->
            _Inserted = ets:insert(systemInfoTable, Rec),
            ok
    end,
    run()
.

% create(socketConnected) ->
%     create(socketConnected, [], "Socket connected ")
% ;
% create(socketLoggedIn, {LoginMessage}) ->
%     create(socketLoggedIn, [], LoginMessage)
% ;
% create(socketError, {Reason}) ->
%     create(socketError, [], Reason)
% ;
% create(socketClosed, {_}) ->
%     create(socketClosed, [], "Socket closed")
% ;
% create(socketReadLine, {Count}) ->
%     create(socketReadLine, Count, "APRS Line counter")
% .

create(InfoType, Datas, Message) ->
    Rec = #systemInfo{ 
        infoType = InfoType,
        time = calendar:universal_time(), 
        datas = Datas,
        message = Message
    },
    whereis(systeminfodb) ! {create, Rec}
.

list() ->
    ets:tab2list(systemInfoTable)
.    

count() ->
    length(ets:tab2list(systemInfoTable))
.
