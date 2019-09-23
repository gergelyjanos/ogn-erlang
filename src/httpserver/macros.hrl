
-define(HTTPRequestLog(SessionId), ogn_collector_logger:debug("HTTP request: ~p ~p ~p~n", [?MODULE, ?FUNCTION_NAME, SessionId])).
-define(JSONHEADER, ["Content-Type: application/json\r\n\r\n"]).
