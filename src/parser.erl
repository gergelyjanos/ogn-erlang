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
-define(timestamp, Hour, Minute, Second).

-define(AIRCRAFT_PATTERN, "^(?P<device>.+)>(?P<message_format>.+),.+,(?P<receiver>.+):\\/(?P<timestamp>\\d+)h(?P<latitude>\\d+\\.\\d+[NS])[\\/\\\\]+(?P<longitude>\\d+\\.\\d+[EW])(?P<heading>\\S\\d+)\\/(?P<ground_speed>\\d+)\\/A=(?P<altitude>\\d+)\\s+(?P<comment>.*)\\s*$").
-define(AIRCRAFTCOMMENT_PATTERN, "^!\\S+!\\s+(?P<device_id>id[0-9a-fA-F]{8})\\s+(?P<climb_rate>[+-]*\\d+.p.)\\s+(?P<turn_rate>[+-]*[\\d\\.]+rot)\\s*.*$").
-define(RECEIVERPOSITION_PATTERN, "^(?P<receiver>.+)>(?P<message_format>.+),.+,.+,(?P<server>.+):\\/(?P<timestamp>\\d+)h(?P<latitude>\\d+\\.\\d+[NS])[\\/\\\\I]+(?P<longitude>\\d+\\.\\d+[EW])[\\/\\&]+A=(?P<altitude>\\d+)$").
-define(RECEIVERSTATUS_PATTERN, "^(?P<receiver>.+)>(?P<message_format>.+),.+,.+,(?P<server>.+):>(?P<timestamp>\\d+)h\\s+(?P<version>.+)\\s+CPU:(?P<cpu>\\d+\\.\\d+)\s+RAM:(?P<ram>\\S+)\\s+(?P<other>.*)$").
-define(TIMESTAMP_PATTERN, "^(\\d\\d)(\\d\\d)(\\d\\d)$").


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
            AircraftPosition = #aircraftPosition{
                device = Device, 
                messageFormat = MessageFormat,
                receiver = Receiver,
                time = timestampToDateTime(Timestamp),
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
            {match, AircraftPosition};
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
                time = timestampToDateTime(Timestamp),
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
                time = timestampToDateTime(Timestamp),
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

timestampToDateTime(Timestamp) ->
    {ok, Regex} = re:compile(?TIMESTAMP_PATTERN, [unicode]),
    case re:run(Timestamp, Regex, [{capture, all_but_first, list}]) of 
        {match, [?timestamp]} ->
            {DateNow, TimeNow} = calendar:universal_time(),
            SecondsNow = calendar:time_to_seconds(TimeNow),
            Time = {list_to_integer(Hour), list_to_integer(Minute), list_to_integer(Second)},
            SecondsTime = calendar:time_to_seconds(Time),
            if
                SecondsNow < SecondsTime -> 
                    DaysNow = calendar:date_to_gregorian_days(DateNow),
                    DateYesterday = calendar:gregorian_days_to_date(DaysNow - 1),
                    {DateYesterday, Time};
                true ->
                    {DateNow, Time}
            end;
        nomatch -> 
            calendar:universal_time()
    end
.
