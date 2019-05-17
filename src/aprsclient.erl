%% @author Janos Gergely <gergelyjanos@td1990.org>
%% @doc Module to receive APRS beacons from tcp socket
%% @copyright 2019 TD*1990
%% @version 1.0.0

-module(aprsclient).
-export([start/1]).

-define(AprsPort, 10152).
-define(AprsHost, "aprs.glidernet.org").
-define(LoginMessage, "user td1990 pass -1 vers ogn_erlang 1.0.0\r\n").
-define(PassivemodeConnectOptions,	[binary, {active, false}, {packet, line}, {delay_send, false}, {nodelay, true}]).
-define(ActivemodeConnectOptions,	[binary, {active, true}, {packet, line}, {delay_send, false}, {nodelay, true}]).
-define(Timeout, 20000).
-define(ReconnectSleep, 10000).

%% @doc Public function to run the TCP reader for read lines from socket and send to parser.
start(Parser) ->
    runpassivemode(connect(), Parser, 0)
.

connect() ->
	connect(true)
.

connect(First) ->
    case gen_tcp:connect(?AprsHost, ?AprsPort, ?PassivemodeConnectOptions, ?Timeout) of
		{ok, Socket} ->
			systemInfoDb:create("socketConnected", Socket),
			io:format("Socket connected: ~p~n", [Socket]),
			{ok, ServerName} = gen_tcp:recv(Socket, 0),
			io:format("Received ServerName: ~p~n", [ServerName]),
			ok = gen_tcp:send(Socket, ?LoginMessage),
			io:format("Logged in: ~p~n", [?LoginMessage]),
			systemInfoDb:create("socketLoggedIn", ?LoginMessage),
			Socket;
		{error, Reason} ->
			if 
				First == true -> 	
					systemInfoDb:create("socketError", Reason),
					io:format("Socket connect error: ~p~n", [Reason]);
				First == false ->
					ok
			end,
			timer:sleep(?ReconnectSleep),
			connect(false)
	end
.

%% @doc Private function to read lines for ever and send to the parser.
runpassivemode(Socket, Parser, Counter) ->
    Socket2 = case gen_tcp:recv(Socket, 0, ?Timeout) of
	    {ok, Line} ->
			Comment = string:prefix(Line, "#"),
			if 
				Comment == nomatch -> 
					Parser ! {line, Line};
				true -> 
					sendkeepalive(Socket),
					Parser ! {comment, Comment}
			end,
			Socket;
    	{error, timeout} ->
			io:format("Socket timedout: ~n"),
			sendkeepalive(Socket),
			Socket;
		{error, closed} -> 
			systemInfoDb:create("socketClosed", ""),
			io:format("Socket closed~n"),
			connect();
		{error, Reason} ->
			systemInfoDb:create("socketError", Reason),
			io:format("Socket error ~p~n", [Reason]),
			Socket
    end,

	if 
		Counter rem 1000 == 0 ->
			systemInfoDb:create("socketReadLine", Counter),
			io:format("Socket reads ~pk~n", [Counter div 1000]);
		true ->
			ok
	end,

	runpassivemode(Socket2, Parser, Counter + 1)
.

%% @doc Private function to send #keepalive message to the server.
sendkeepalive(Socket) ->
    gen_tcp:send(Socket, "#keepalive")
.

% runactivemode(connected, Parser) ->
% 	receive
% 		{tcp, Socket, Line} ->
% 		    io:format("Received ServerName: ~p~n", [Line]),
% 		    ok = gen_tcp:send(Socket, ?LoginMessage),
%     		io:format("Logged in: ~p~n", [?LoginMessage]),
% 			runactivemode(loggedin, Parser);
% 		{tcp_closed, Socket} ->
% 			io:format("Socket closed~n"),
% 			connect(),
% 			runactivemode(connected, Parser);
% 		{tcp_error, Socket, Reason} ->
% 			io:format("Socket error ~p~n", [Reason])
% 	end
% ;
% runactivemode(loggedin, Parser) ->
% 	receive
% 		{tcp, Socket, Line} ->
% 			Comment = string:prefix(Line, "#"),
% 			if 
% 				Comment == nomatch -> 
% 					Parser ! {line, Line};
% 				true -> 
% 					sendkeepalive(Socket),
% 					Parser ! {comment, Comment}
% 			end;
% 			runactivemode(loggedin, Parser);
% 		{tcp_closed, Socket} ->
% 			io:format("Socket closed~n"),
% 			connect(),
% 			runactivemode(connected, Parser);
% 		{tcp_error, Socket, Reason} ->
% 			io:format("Socket error ~p~n", [Reason])
% 	end

% .

% connect() ->
%     {ok, Socket} = gen_tcp:connect(?AprsHost, ?AprsPort, ?ActivemodeConnectOptions),
%     io:format("Socket connected: ~p~n", [Socket]),
% 	connected
% .

