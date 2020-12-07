-module(ogn_parser_aircraft_position_parser).

-export([create_aircraft_position_record/2]).

-include("./ogn_parser_pattern.hrl").

-define(RUN_OPTIONS, [{capture, all_but_first, list}]).

create_aircraft_position_record(LineValues, Parsers) ->
    create_record(LineValues, Parsers, #{}).

%% @private
create_record([], _, Record) -> 
    Record;
create_record([{?TIMESTAMP_KEY, Value} | Tail], Parsers, Record) ->
    %% todo timestamp
    create_record(Tail, Parsers, Record#{?TIMESTAMP_KEY => Value});
create_record([{?LATITUDE_KEY = Key, Value} | Tail], #{latlon := LatLonParser} = Parsers, Record) ->
    Lat = ogn_parser_tool:list_to_latlon(Value, LatLonParser),
    create_record(Tail, Parsers, Record#{Key => {Lat, Value}});
create_record([{?LONGITUDE_KEY = Key, Value} | Tail], #{latlon := LatLonParser} = Parsers, Record) ->
    Lon = ogn_parser_tool:list_to_latlon(Value, LatLonParser),
    create_record(Tail, Parsers, Record#{Key => {Lon, Value}});
create_record([{?HEADING_KEY = Key, Value} | Tail], Parsers, Record) ->
    Heading = {list_to_integer(Value), d},
    create_record(Tail, Parsers, Record#{Key => Heading});
create_record([{?GROUND_SPEED_KEY = Key, Value} | Tail], Parsers, Record) ->
    GroundSpeed = {list_to_integer(Value), kmph},
    create_record(Tail, Parsers, Record#{Key => GroundSpeed});
create_record([{?ALTITUDE_KEY = Key, Value} | Tail], Parsers, Record) ->
    Altitude = {list_to_integer(Value), foot},
    create_record(Tail, Parsers, Record#{Key => Altitude});
create_record([{?COMMENT_KEY, Value} | Tail], #{aircraft_comment := CommentParser} = Parsers, Record) ->
    {DeviceId, ClimbRate, TurnRate} = 
        parse_aircraft_comment(Value, CommentParser),
    create_record(Tail, Parsers, Record#{
        ?DEVICE_ID_KEY => DeviceId,
        ?CLIMB_RATE_KEY => ClimbRate,
        ?TURN_RATE_KEY => TurnRate
    });
create_record([{Key, Value} | Tail], Parsers, Record) ->
    create_record(Tail, Parsers, Record#{Key => Value}).

%% @private
parse_aircraft_comment(Data, {Regex, _}) ->
    case re:run(Data, Regex, ?RUN_OPTIONS) of
        {match, [DeviceId, ClimbRate, TurnRate]} ->
            {DeviceId, {list_to_integer(ClimbRate), footpmin}, {list_to_float(TurnRate), tr}};
        nomatch -> 
            {"", {0.0, footpmin}, {0.0, tr}}
    end.
