-module(web).

-export([index/3]).

% http://localhost:8080/web/index
index(SessionID, _Env, _Input) -> 
    % ?HTTPRequestLog(SessionID),
    mod_esi:deliver(SessionID, ["Content-Type: text/html\r\n\r\n"]),
    mod_esi:deliver(SessionID, [
        "<html><body><h1>Hello OGN Erlang!</h1>",
        "<div><ul>", 
        "<li><a href=""/api/aircraftpositionapi/list"">Aircraft position list [", 
        % io_lib:format("~p", [aircraftPositionDb:count()]), 
        "]</a></li>",
        "<li><a href=""/api/aircraftpositionapi/count"">Aircraft count</a></li>",
        "<li><a href=""/api/receiverpositionapi/list"">Receiver position list [",
        % io_lib:format("~p", [receiverPositionDb:count()]),
        "]</a></li>",
        "<li><a href=""/api/receiverpositionapi/count"">Receiver count</a></li>",
        "<li><a href=""/api/receiverpositionapi/receiver/NkovJ7"">NkovJ7 receiver position</a></li>",
        "<li><a href=""/api/systeminfoapi/list"">System Info</a></li>",
        "</ul></div>",
        "</body></html>" 
    ])
.
