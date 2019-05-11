%% @author Janos Gergely <gergelyjanos@td1990.org>
%% @doc Function to parse APRS messages
%% @copyright 2019 TD*1990
%% @version 1.0.0

-module(parser).
-export([run/0]).
-include("parser_records.hrl").

-define(aircraftData, Device, MessageFormat, Receiver, Timestamp, Latitude, Longitude, Heading, GroundSpeed, Altitude, Comment).
-define(aircraftAdditionalData, DeviceId, ClimbRate, TurnRate).
-define(receiverPosition, Receiver, MessageFormat, Server, Timestamp, Latitude, Longitude, Altitude).
-define(receiverStatus, Receiver, MessageFormat, Server, Timestamp, Version, Cpu, Ram, Other).
-define(AIRCRAFT_PATTERN, "^(?P<device>.+)>(?P<message_format>.+),.+,(?P<receiver>.+):\\/(?P<timestamp>\\d+)h(?P<latitude>\\d+\\.\\d+[NS])[\\/\\\\]+(?P<longitude>\\d+\\.\\d+[EW])(?P<heading>\\S\\d+)\\/(?P<ground_speed>\\d+)\\/A=(?P<altitude>\\d+)\\s+(?P<comment>.*)\\s*$").
-define(AIRCRAFTCOMMENT_PATTERN, "^!\\S+!\\s+(?P<device_id>id[0-9a-fA-F]{8})\\s+(?P<climb_rate>[+-]*\\d+.p.)\\s+(?P<turn_rate>[+-]*[\\d\\.]+rot)\\s*.*$").
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
    case parseline(aircraft, string:chomp(Line)) of
        {match, Data} ->
            io:format("~p~n", [Data]);
        nomatch -> 
            io:format("Line parser nomatch ~p~n", [Line])
    end
.

%% @doc Private function to parse line.
parseline(aircraft, Line) ->
    {ok, AircraftRegex} = re:compile(?AIRCRAFT_PATTERN, [unicode]),
    case re:run(Line, AircraftRegex, [{capture, all_but_first, list}]) of 
        {match, [?aircraftData]} ->
            [?aircraftAdditionalData] = parseAircraftAdditionalData(Comment),
            Aircraft = #aircraft{
                device = Device, 
                messageFormat = MessageFormat,
                receiver = Receiver,
                timestamp = Timestamp,
                latitude = Latitude,
                longitude = Longitude,
                heading = Heading,
                groundSpeed = GroundSpeed,
                altitude = Altitude,
                deviceId = DeviceId,
                climbRate = ClimbRate,
                turnRate = TurnRate
            },
            % todo send aircraft data to DB
            {match, Aircraft};
        nomatch ->
            parseline(receiverPosition, Line)
    end
;
parseline(receiverPosition, Line) ->
    {ok, Regex} = re:compile(?RECEIVERPOSITION_PATTERN, [unicode]),
    case re:run(Line, Regex, [{capture, all_but_first, list}]) of 
        {match, [?receiverPosition]} ->
            ReceiverPosition = #receiverPosition{
                receiver = Receiver,
                messageFormat = MessageFormat,
                server = Server,
                timestamp = Timestamp,
                latitude = Latitude,
                longitude = Longitude,
                altitude = Altitude
            },
            {match, ReceiverPosition};
        nomatch -> 
            parseline(receiverStatus, Line)
    end
;
parseline(receiverStatus, Line) ->
    {ok, Regex} = re:compile(?RECEIVERSTATUS_PATTERN, [unicode]),
    case re:run(Line, Regex, [{capture, all_but_first, list}]) of 
        {match, [?receiverStatus]} ->
            ReceiverStatus = #receiverStatus{
                receiver = Receiver,
                messageFormat = MessageFormat,
                server = Server,
                timestamp = Timestamp,
                version = Version,
                cpu = Cpu,
                ram = Ram,
                other = Other
            },
            {match, ReceiverStatus};
        nomatch -> 
            nomatch
    end
.

parseAircraftAdditionalData(Comment) ->
    {ok, Regex} = re:compile(?AIRCRAFTCOMMENT_PATTERN, [unicode]),
    case re:run(Comment, Regex, [{capture, all_but_first, list}]) of 
        {match, [?aircraftAdditionalData]} ->
            [?aircraftAdditionalData];
        nomatch -> 
            ["", "", ""]
    end
.


%% @doc Private function to parse comment.
parsecomment(Comment) ->
	io:format("Comment parser ~p~n", [Comment])
.	
