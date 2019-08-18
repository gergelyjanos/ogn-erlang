%%%--------------------------------------------------------------------- 
%% @doc ogn_collector public API
%% @end
%%%--------------------------------------------------------------------- 

-module(ogn_collector_app).

-behavior(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    ogn_collector_sup:start_link().

stop(_State) ->
    ok.

% main() ->
%     SystemInfoDb = systemInfoDb:spawnandstart(),
%     io:format("SystemInfoDb Pid ~p~n", [SystemInfoDb]),

%     HttpServer = spawn(httpserver, start, []),
%     register(httpserver, HttpServer),
%     io:format("HttpServer Pid ~p~n", [HttpServer]),

%     AircraftPositionDb = spawn(aircraftPositionDb, start, []),
%     register(aircraftPositionDb, AircraftPositionDb),
%     io:format("AircraftPositionDb Pid ~p~n", [AircraftPositionDb]),

%     ReceiverPositionDb = spawn(receiverPositionDb, start, []),
%     register(receiverPositionDb, ReceiverPositionDb),
%     io:format("ReceiverPositionDb Pid ~p~n", [ReceiverPositionDb]),

%     Parser = spawn(parser, start, []),
%     io:format("Parser Pid ~p~n", [Parser]),

%     Client = spawn(aprsclient, start, [Parser]),
%     io:format("Client Pid ~p~n", [Client]),
%     % {ok, null}
%     io:read("running?")
% .
