%% @author Janos Gergely <gergelyjanos@td1990.org>
%% @doc Module for web api
%% @copyright 2019 TD*1990
%% @version 1.0.0

-module(aircraftpositionapi).
-export([list/3, count/3]).
-include("macros.hrl").
-include("aircraftPositionRecord.hrl").

-define(HEADER, ["Content-Type: application/json\r\n\r\n"]).

% http://localhost:8080/api/aircraftpositionapi/list
list(SessionID, _Env, _Input) -> 
    ?HTTPRequestLog(SessionID),
    % io:format("HTTP request: ~p ~p~n", [?MODULE, ?FUNCTION_NAME]),
    DbList = aircraftPositionDb:list(),
    mod_esi:deliver(SessionID, ?HEADER), 
    mod_esi:deliver(SessionID, [json:listOfRecordsToJson(record_info(fields, aircraftPosition), DbList)])
.

% http://localhost:8080/api/aircraftpositionapi/count
count(SessionID, _Env, _Input) -> 
    ?HTTPRequestLog(SessionID),
    % io:format("HTTP request: ~p ~p~n", [?MODULE, ?FUNCTION_NAME]),
    mod_esi:deliver(SessionID, ?HEADER), 
    mod_esi:deliver(SessionID, [json:integerToJson("count", aircraftPositionDb:count())])
.

