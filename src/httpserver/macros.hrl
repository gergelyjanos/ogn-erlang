
-define(HTTPRequestLog(SessionId), ?LOG_DEBUG("HTTP request: ~p ~p ~p", [?MODULE, ?FUNCTION_NAME, SessionId])).
-define(JSONHEADER, ["Content-Type: application/json\r\n\r\n"]).
