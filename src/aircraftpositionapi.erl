%% @author Janos Gergely <gergelyjanos@td1990.org>
%% @doc Module for web api
%% @copyright 2019 TD*1990
%% @version 1.0.0

-module(aircraftpositionapi).
-export([list/3]).

% http://localhost:8080/api/aircraftpositionapi/list
list(SessionID, _Env, _Input) -> 
    mod_esi:deliver(SessionID, ["Content-Type: text/html\r\n\r\n", "<html><body>Hello aircraft! all data</body></html>" ])
.


