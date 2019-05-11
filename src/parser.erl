%% @author Janos Gergely <gergelyjanos@td1990.org>
%% @doc Function to parse APRS messages
%% @copyright 2019 TD*1990
%% @version 1.0.0

-module(parser).
-export([run/0]).

-define(beaconData, Device, MessageFormat, Receiver, Timestamp, Latitude, Longitude, Heading, GroundSpeed, Altitude, Comment).
-define(receiverPosition, Receiver, MessageFormat, Server, Timestamp, Latitude, Longitude, Altitude).
-define(receiverStatus, Receiver, MessageFormat, Server, Timestamp, Version, Cpu, Ram, Other).
-define(AIRCRAFT_PATTERN, "^(?P<device>.+)>(?P<message_format>.+),.+,(?P<receiver>.+):\\/(?P<timestamp>\\d+)h(?P<latitude>\\d+\\.\\d+[NS])[\\/\\\\]+(?P<longitude>\\d+\\.\\d+[EW])(?P<heading>\\S\\d+)\\/(?P<ground_speed>\\d+)\\/A=(?P<altitude>\\d+)\\s(?P<comment>.*)$").
-define(RECEIVERPOSITION_PATTERN, "^(?P<receiver>.+)>(?P<message_format>.+),.+,.+,(?P<server>.+):\\/(?P<timestamp>\\d+)h(?P<latitude>\\d+\\.\\d+[NS])[\\/\\\\I]+(?P<longitude>\\d+\\.\\d+[EW])[\\/\\&]+A=(?P<altitude>\\d+)$").
-define(RECEIVERSTATUS_PATTERN, "^(?P<receiver>.+)>(?P<message_format>.+),.+,.+,(?P<server>.+):>(?P<timestamp>\\d+)h\\s+(?P<version>.+)\\s+CPU:(?P<cpu>\\d+\\.\\d+)\s+RAM:(?P<ram>\\S+)\\s+(?P<other>.*)$").

%% @doc Public function to parse messages.
run() ->
    receive 
        {line, Line} -> parseline(Line), run();
        {comment, Comment} -> parsecomment(Comment), run();
        {close} -> io:format("Close parser~n")
    end
.	

parseline(Line) -> 
    ParseStatus = parseline(aircraft, string:chomp(Line)),
    if 
        ParseStatus == nomatch -> 
            io:format("Line parser nomatch ~p~n", [Line]);
        true -> true
    end
.

%% @doc Private function to parse line.
parseline(aircraft, Line) ->
    {ok, AircraftRegex} = re:compile(?AIRCRAFT_PATTERN, [unicode]),
    case re:run(Line, AircraftRegex, [{capture, all_but_first, list}]) of 
        {match, [?beaconData]} ->
            % todo parse more data from comment
            % todo send aircraft data to DB
            io:format("Aircraft match ~p~n", [[Device, Receiver, Latitude, Longitude, Heading, GroundSpeed]]),
            match;
        nomatch ->
            parseline(receiverPosition, Line)
    end
;
parseline(receiverPosition, Line) ->
    {ok, Regex} = re:compile(?RECEIVERPOSITION_PATTERN, [unicode]),
    case re:run(Line, Regex, [{capture, all_but_first, list}]) of 
        {match, [?receiverPosition]} ->
            % todo send receiver position data to DB
            io:format("Receiver position match ~p~n", [[Receiver]]),
            match;
        nomatch -> 
            parseline(receiverStatus, Line)
    end
;
parseline(receiverStatus, Line) ->
    {ok, Regex} = re:compile(?RECEIVERSTATUS_PATTERN, [unicode]),
    case re:run(Line, Regex, [{capture, all_but_first, list}]) of 
        {match, [?receiverStatus]} ->
            % todo send receiver status data to DB
            io:format("Receiver status match ~p~n", [[Receiver, Cpu, Ram]]),
            match;
        nomatch -> 
            nomatch
    end
.

%% @doc Private function to parse comment.
parsecomment(Comment) ->
	io:format("Comment parser ~p~n", [Comment])
.	
