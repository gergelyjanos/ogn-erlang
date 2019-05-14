%% @author Janos Gergely <gergelyjanos@td1990.org>
%% @doc Module for web api
%% @copyright 2019 TD*1990
%% @version 1.0.0

-module(receiverpositionapi).
-export([list/3, count/3]).
-include("macros.hrl").
-include("receiverPositionRecord.hrl").

-define(HEADER, ["Content-Type: application/json\r\n\r\n"]).

% http://localhost:8080/api/receiverpositionapi/list
list(SessionID, _Env, _Input) -> 
    ?HTTPRequestLog(SessionID),
    DbList = receiverPositionDb:list(),
    mod_esi:deliver(SessionID, ?HEADER), 
    mod_esi:deliver(SessionID, [json:listOfRecordsToJson(record_info(fields, receiverPosition), DbList)])
.

% http://localhost:8080/api/receiverpositionapi/count
count(SessionID, _Env, _Input) -> 
    ?HTTPRequestLog(SessionID),
    mod_esi:deliver(SessionID, ?HEADER), 
    mod_esi:deliver(SessionID, [json:integerToJson("count", receiverPositionDb:count())])
.
