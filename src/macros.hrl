
-define(HTTPRequestLog(SessionId), io:format("HTTP request: ~p ~p ~p~n", [?MODULE, ?FUNCTION_NAME, SessionId])).
