%% @author Janos Gergely <gergelyjanos@td1990.org>
%% @doc Module to parse APRS messages
%% @copyright 2019 TD*1990
%% @version 1.0.0

-module(parser).
-export([start/0]).
-include("aircraftPositionRecord.hrl").
-include("receiverPositionRecord.hrl").
-include("receiverStatusRecord.hrl").

-define(aircraftData, Device, MessageFormat, Receiver, Timestamp, Latitude, Longitude, Heading, GroundSpeed, Altitude, Comment).
-define(aircraftAdditionalData, DeviceId, ClimbRate, TurnRate).
-define(receiverPosition, Receiver, MessageFormat, Server, Timestamp, Latitude, Longitude, Altitude).
-define(receiverStatus, Receiver, MessageFormat, Server, Timestamp, Version, Cpu, Ram, Other).
-define(timestamp, Hour, Minute, Second).

-define(AIRCRAFT_PATTERN, "^(?P<device>.+)>(?P<message_format>.+),.+,(?P<receiver>.+):\\/(?P<timestamp>\\d+)h(?P<latitude>\\d+\\.\\d+[NS])[\\/\\\\]+(?P<longitude>\\d+\\.\\d+[EW])\\S(?P<heading>\\d+)\\/(?P<ground_speed>\\d+)\\/A=(?P<altitude>\\d+)\\s+(?P<comment>.*)\\s*$").
-define(AIRCRAFTCOMMENT_PATTERN, "^!\\S+!\\s+(?P<device_id>id[0-9a-fA-F]{8})\\s+(?P<climb_rate>[+-]*\\d+).p.\\s+(?P<turn_rate>[+-]*[\\d\\.]+)rot\\s*.*$").
-define(RECEIVERPOSITION_PATTERN, "^(?P<receiver>.+)>(?P<message_format>.+),.+,.+,(?P<server>.+):\\/(?P<timestamp>\\d+)h(?P<latitude>\\d+\\.\\d+[NS])[\\/\\\\I]+(?P<longitude>\\d+\\.\\d+[EW])[\\/\\&]+A=(?P<altitude>\\d+)$").
-define(RECEIVERSTATUS_PATTERN, "^(?P<receiver>.+)>(?P<message_format>.+),.+,.+,(?P<server>.+):>(?P<timestamp>\\d+)h\\s+(?P<version>.+)\\s+CPU:(?P<cpu>\\d+\\.\\d+)\s+RAM:(?P<ram>\\S+)\\s+(?P<other>.*)$").
-define(TIMESTAMP_PATTERN, "^(\\d\\d)(\\d\\d)(\\d\\d)$").

-record(regexps, 
    {
        aircraft, 
        receiverPosition,
        receiverStatus,
        aircraftComment,
        timestamp
    }).

%% @doc Public function to parse messages.
start() ->
    Regexps = compileRegexp(),
    run(Regexps)
.

run(Regexps)->
    receive 
        {close} -> 
            io:format("Close parser~n");
        {line, Line} -> 
            LineParser = spawn(fun() -> parseline(Regexps) end),
            LineParser ! Line,
            run(Regexps);
        {comment, Comment} -> 
            parsecomment(Comment), 
            run(Regexps)
    end
.	

compileRegexp() ->
    {ok, AircraftRegex} = re:compile(?AIRCRAFT_PATTERN, [unicode]),
    {ok, ReceiverPositionRegex} = re:compile(?RECEIVERPOSITION_PATTERN, [unicode]),
    {ok, ReceiverStatusRegex} = re:compile(?RECEIVERSTATUS_PATTERN, [unicode]),
    {ok, AircraftCommentRegex} = re:compile(?AIRCRAFTCOMMENT_PATTERN, [unicode]),
    {ok, TimestampRegex} = re:compile(?TIMESTAMP_PATTERN, [unicode]),
    #regexps{
        aircraft = AircraftRegex,
        receiverPosition = ReceiverPositionRegex,
        receiverStatus = ReceiverStatusRegex,
        aircraftComment = AircraftCommentRegex,
        timestamp = TimestampRegex
    }
.

parseline(Regexps) -> 
    receive
        Line ->
            % io:format("[~p] ~p~n", [self(), Line]),
            case parseline(aircraft, string:chomp(Line), Regexps) of
                {aircraftPosition, AircraftPosition} ->
                    Db = whereis(aircraftPositionDb),
                    Db ! {create, AircraftPosition};
                {receiverPosition, ReceiverPosition} ->
                    Db = whereis(receiverPositionDb),
                    Db ! {create, ReceiverPosition};
                {receiverStatus, _ReceiverStatus} ->
                    _Db = whereis(receiverStatusDb);
                    % Db ! {createorupdate, ReceiverStatus};
                nomatch -> 
                    nomatch
            end
    end
.

%% @doc Private function to parse line.
parseline(aircraft, Line, Regexps) ->
    Regex = Regexps#regexps.aircraft,
    case re:run(Line, Regex, [{capture, all_but_first, list}]) of 
        {match, [?aircraftData]} ->
            [?aircraftAdditionalData] = parseAircraftAdditionalData(Comment, Regexps),
            AircraftPosition = #aircraftPosition{
                device = Device, 
                messageFormat = MessageFormat,
                receiver = Receiver,
                time = timestampToDateTime(Timestamp, Regexps),
                latitude = latlonParser(lat, Latitude),
                longitude = latlonParser(lon, Longitude),
                heading = {list_to_integer(Heading), d},
                groundSpeed = {list_to_integer(GroundSpeed), kmph},
                altitude = {list_to_integer(Altitude), f},
                deviceId = DeviceId,
                climbRate = {list_to_integer(ClimbRate), fpm},
                turnRate = {list_to_float(TurnRate), rot}
            },
            {aircraftPosition, AircraftPosition};
        nomatch ->
            parseline(receiverPosition, Line, Regexps)
    end
;
parseline(receiverPosition, Line, Regexps) ->
    Regex = Regexps#regexps.receiverPosition,
    case re:run(Line, Regex, [{capture, all_but_first, list}]) of 
        {match, [?receiverPosition]} ->
            ReceiverPosition = #receiverPosition{
                receiver = Receiver,
                messageFormat = MessageFormat,
                server = Server,
                time = timestampToDateTime(Timestamp, Regexps),
                latitude = latlonParser(lat, Latitude),
                longitude = latlonParser(lon, Longitude),
                altitude = {list_to_integer(Altitude), f}
            },
            {receiverPosition, ReceiverPosition};
        nomatch -> 
            parseline(receiverStatus, Line, Regexps)
    end
;
parseline(receiverStatus, Line, Regexps) ->
    Regex = Regexps#regexps.receiverStatus,
    case re:run(Line, Regex, [{capture, all_but_first, list}]) of 
        {match, [?receiverStatus]} ->
            ReceiverStatus = #receiverStatus{
                receiver = Receiver,
                messageFormat = MessageFormat,
                server = Server,
                time = timestampToDateTime(Timestamp, Regexps),
                version = Version,
                cpu = Cpu,
                ram = Ram,
                other = Other
            },
            {receiverStatus, ReceiverStatus};
        nomatch -> 
            nomatch
    end
.

parseAircraftAdditionalData(Comment, Regexps) ->
    Regex = Regexps#regexps.aircraftComment,
    case re:run(Comment, Regex, [{capture, all_but_first, list}]) of 
        {match, [?aircraftAdditionalData]} ->
            [?aircraftAdditionalData];
        nomatch -> 
            ["", "0", "0.0"]
    end
.


%% @doc Private function to parse comment.
parsecomment(_Comment) ->
    ok
	% io:format("Comment parser ~p~n", [Comment])
.	

timestampToDateTime(Timestamp, Regexps) ->
    Regex = Regexps#regexps.timestamp,
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

latlonParser(lat, Line) ->
    % DDMM.HHN
    % 01234567
    Degree = list_to_integer(string:slice(Line, 0, 2)),
    Minute = list_to_integer(string:slice(Line, 2, 2)),
    Second = list_to_integer(string:slice(Line, 5, 2)),
    Globe = string:slice(Line, 7, 1),
    DD = Degree + (Minute / 60) + (Second / 3600),
    if 
        Globe == "N" ->
            {DD, degree, Line};
        Globe == "S" ->
            {-DD, degree, Line}
    end;
latlonParser(lon, Line) ->
    % DDDMM.HHN
    % 012345678
    Degree = list_to_integer(string:slice(Line, 0, 3)),
    Minute = list_to_integer(string:slice(Line, 3, 2)),
    Second = list_to_integer(string:slice(Line, 6, 2)),
    Globe = string:slice(Line, 8, 1),
    DD = Degree + (Minute / 60) + (Second / 3600),
    if 
        Globe == "E" ->
            {DD, degree, Line};
        Globe == "W" ->
            {-DD, degree, Line}
    end
.
