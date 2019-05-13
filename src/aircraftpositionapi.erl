%% @author Janos Gergely <gergelyjanos@td1990.org>
%% @doc Module for web api
%% @copyright 2019 TD*1990
%% @version 1.0.0

-module(aircraftpositionapi).
-export([list/3]).

% http://localhost:8080/api/aircraftpositionapi/list
list(SessionID, _Env, _Input) -> 
    % DbList = "akarmi", % aircraftPositionDb:list(),
    % io:format("api ~p~n", DbList),
    mod_esi:deliver(SessionID, ["Content-Type: text/html\r\n\r\n", "<html><body><pre>akarmi</pre></body></html>" ])
.


