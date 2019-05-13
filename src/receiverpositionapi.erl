%% @author Janos Gergely <gergelyjanos@td1990.org>
%% @doc Module for web api
%% @copyright 2019 TD*1990
%% @version 1.0.0

-module(receiverpositionapi).
-export([list/3, count/3]).

-define(HEADER, ["Content-Type: application/json\r\n\r\n"]).

% http://localhost:8080/api/receiverpositionapi/list
list(SessionID, _Env, _Input) -> 
    DbList = receiverPositionDb:list(),
    mod_esi:deliver(SessionID, ?HEADER), 
    mod_esi:deliver(SessionID, [
        io_lib:format("~p", [DbList])
    ])
.

% http://localhost:8080/api/receiverpositionapi/count
count(SessionID, _Env, _Input) -> 
    Count = receiverPositionDb:count(),
    mod_esi:deliver(SessionID, ?HEADER), 
    mod_esi:deliver(SessionID, [
        io_lib:format("{\"count\": ~p}\r\n", [Count])
    ])
.
