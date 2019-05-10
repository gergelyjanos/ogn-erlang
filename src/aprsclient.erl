%% @author Janos Gergely <gergelyjanos@td1990.org>
%% @doc Function to receive APRS beacons from tcp socket
%% @copyright 2019 TD*1990
%% @version 1.0.0

-module(aprsclient).
-export([run/0]).
%% -import(gen_tcp, [connect/3, recv/2, send/2]).

-define(AprsPort, 10152).
-define(AprsHost, "aprs.glidernet.org").
-define(LoginMessage, "user td1990 pass -1 vers ogn_erlang 1.0.0\r\n").

run() ->
    {ok, Socket} = gen_tcp:connect(?AprsHost, ?AprsPort, [binary, {active, false}, {packet, line}, {delay_send, false}, {nodelay, true}]),
    io:format("Socket connected: ~p~n", [Socket]),
    {ok, ServerName} = gen_tcp:recv(Socket, 0),
    io:format("Received ServerName: ~p~n", [ServerName]),
    ok = gen_tcp:send(Socket, ?LoginMessage),
    io:format("Logged in: ~p~n", [?LoginMessage]),
    % spawn(aprsclient, startkeepalive, [Socket]),    
    readpassivemode(Socket).
    % readactivemode(Socket).

readpassivemode(Socket) ->
    io:format("Socket waiting~n"),
    case gen_tcp:recv(Socket, 0, 50000) of    
        {ok, Binary} ->
            io:format("Socket received: ~p~n", [Binary]),
            % gen_tcp:send(Socket, "#keepalive"),
            readpassivemode(Socket);
        {error, timeout } ->
            io:format("Socket timedout: ~n"),
            gen_tcp:send(Socket, "#keepalive"),
            readpassivemode(Socket);
        {error, closed} ->
            io:format("Socket closed~n");
        {error, Reason} ->
            io:format("Socket error ~p~n", [Reason]),
            readpassivemode(Socket)
    end.

% startkeepalive(Socket) ->
%     timer:apply_interval(240000, aprsclient, sendkeepalive, [Socket]).

% sendkeepalive(Socket) ->
%     io:format("Socket keepalive~n"),
%     gen_tcp:send(Socket, "#keepalive").

% readactivemode(Socket) ->
%     receive 
%         {tcp, Socket, Message} ->
%             io:format("Received: ~p~n", [Message]),
%             readnext(Socket);
%         {tcp_close, Socket} ->
%             io:format("Socket closed~n")
%     end.

%% {ok, Message} = gen_tcp:recv(Socket, 0),
