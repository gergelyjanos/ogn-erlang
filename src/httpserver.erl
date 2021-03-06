%% @author Janos Gergely <gergelyjanos@td1990.org>
%% @doc Module for HTTP server
%% @copyright 2019 TD*1990
%% @version 1.0.0

-module(httpserver).
-export([start/0, test1/3]).
-include("macros.hrl").

start() ->
    setup(),
    ok
.

setup() ->
    Inet = inets:start(),
    io:format("inet:start ~p~n", [Inet]),
    {ok, Pid} = inets:start(httpd, 
        [
            {modules, [ 
                    mod_esi
                ]
            },
            {port, 8080},
            % {bind_address, "localhost"},
            {server_name, "ogn"},
            {server_root, "."},
            {document_root, "."},
            {erl_script_alias, {"/api", 
                [
                    httpserver, 
                    aircraftpositionapi, 
                    receiverpositionapi,
                    systeminfoapi
                ]}},
            {erl_script_nocache, true},
            {mime_types, [{"html","text/html"}, {"json", "application/json"}]}
        ]
    ),
    io:format("httpd ~p ~p~n", [Pid, httpd:info(Pid)])
.

% http://localhost:8080/api/httpserver/test1
test1(SessionID, _Env, _Input) -> 
    ?HTTPRequestLog(SessionID),
    mod_esi:deliver(SessionID, ["Content-Type: text/html\r\n\r\n"]),
    mod_esi:deliver(SessionID, [
        "<html><body><h1>Hello OGN Erlang!</h1>",
        "<div><ul>", 
        io_lib:format("<li><a href=""/api/aircraftpositionapi/list"">Aircraft position list [~p]</a></li>", [aircraftPositionDb:count()]),
        "<li><a href=""/api/aircraftpositionapi/count"">Aircraft count</a></li>",
        io_lib:format("<li><a href=""/api/receiverpositionapi/list"">Receiver position list [~p]</a></li>", [receiverPositionDb:count()]),
        "<li><a href=""/api/receiverpositionapi/count"">Receiver count</a></li>",
        "<li><a href=""/api/receiverpositionapi/receiver/NkovJ7"">NkovJ7 receiver position</a></li>",
        "<li><a href=""/api/systeminfoapi/list"">System Info</a></li>",
        "</ul></div>",
        "</body></html>" 
    ])
.
