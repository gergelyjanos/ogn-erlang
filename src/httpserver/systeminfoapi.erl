%% @author Janos Gergely <gergelyjanos@td1990.org>
%% @doc Module for web api
%% @copyright 2019 TD*1990
%% @version 1.0.0

-module(systeminfoapi).
-export([list/3]).
-include("../macros.hrl").
-include("../systemInfoRecord.hrl").


% http://localhost:8080/api/systeminfoapi/list
list(SessionID, _Env, _Input) -> 
    ?HTTPRequestLog(SessionID),
    DbList = systemInfoDb:list(),
    mod_esi:deliver(SessionID, ?JSONHEADER), 
    mod_esi:deliver(SessionID, [json:listOfRecordsToJson(record_info(fields, systemInfo), DbList)])
.

