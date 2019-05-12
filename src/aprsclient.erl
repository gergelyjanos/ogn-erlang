%% @author Janos Gergely <gergelyjanos@td1990.org>
%% @doc Function to receive APRS beacons from tcp socket
%% @copyright 2019 TD*1990
%% @version 1.0.0

-module(aprsclient).
-export([start/1]).

-define(AprsPort, 10152).
-define(AprsHost, "aprs.glidernet.org").
-define(LoginMessage, "user td1990 pass -1 vers ogn_erlang 1.0.0\r\n").
-define(ConnectOptions,	[binary, {active, false}, {packet, line}, {delay_send, false}, {nodelay, true}]).

%% @doc Public function to run the TCP reader for read lines from socket and send to parser.
start(Parser) ->
    {ok, Socket} = gen_tcp:connect(?AprsHost, ?AprsPort, ?ConnectOptions),
    io:format("Socket connected: ~p~n", [Socket]),
    {ok, ServerName} = gen_tcp:recv(Socket, 0),
    io:format("Received ServerName: ~p~n", [ServerName]),
    ok = gen_tcp:send(Socket, ?LoginMessage),
    io:format("Logged in: ~p~n", [?LoginMessage]),
    runpassivemode(Socket, Parser)
.

%% @doc Private function to read lines for ever and send to the parser.
runpassivemode(Socket, Parser) ->
    Result = case gen_tcp:recv(Socket, 0, 50000) of
	    {ok, Line} ->
			Comment = string:prefix(Line, "#"),
			if 
				Comment == nomatch -> 
					Parser ! {line, Line};
				true -> 
					sendkeepalive(Socket),
					Parser ! {comment, Comment}
			end;
    	{error, timeout} ->
			io:format("Socket timedout: ~n"),
			sendkeepalive(Socket);
		{error, closed} -> 
			io:format("Socket closed~n"),
			Parser ! {close},
			stopRuning;
		{error, Reason} ->
			io:format("Socket error ~p~n", [Reason])
    end,
	if 
		Result == stopRuning ->
			socketClosed;
		true->	
			runpassivemode(Socket, Parser)
	end
.

%% @doc Private function to send #keepalive message to the server.
sendkeepalive(Socket) ->
    gen_tcp:send(Socket, "#keepalive")
.

