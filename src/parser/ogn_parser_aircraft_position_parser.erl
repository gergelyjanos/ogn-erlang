-module(ogn_parser_aircraft_position_parser).

-export([create_aircraft_position_record/2]).

-include("./ogn_parser_pattern.hrl").

-define(RUN_OPTIONS, [{capture, all_but_first, list}]).

create_aircraft_position_record(LineValues, Parsers) ->
    create_record(LineValues, Parsers, #{geo_coord => #{}, speed => #{}}).

%% @private
create_record([], _, Record) -> 
    Record;
create_record([{?DEVICE_KEY, Value} | Tail], Parsers, Record) ->
    create_record(Tail, Parsers, Record#{device => Value, aircraft_id => Value});
create_record([{?DEVICE_ID_KEY, Value} | Tail], Parsers, Record) ->
    create_record(Tail, Parsers, Record#{device_id => Value});
create_record([{?RECEIVER_KEY, Value} | Tail], Parsers, Record) ->
    create_record(Tail, Parsers, Record#{receiver => Value});
create_record([{?MESSAGE_FORMAT_KEY, Value} | Tail], Parsers, Record) ->
    create_record(Tail, Parsers, Record#{message_format => Value});
create_record([{?TIMESTAMP_KEY, Value} | Tail], Parsers, Record) ->
    %% todo timestamp
    create_record(Tail, Parsers, Record#{timestamp => Value});
create_record([{?LATITUDE_KEY, Value} | Tail], #{latlon := LatLonParser} = Parsers, #{geo_coord := GeoCoord} = Record) ->
    Lat = ogn_parser_tool:list_to_latlon(Value, LatLonParser),
    create_record(Tail, Parsers, Record#{geo_coord := GeoCoord#{latitude => Lat, lat => Value}});
create_record([{?LONGITUDE_KEY, Value} | Tail], #{latlon := LatLonParser} = Parsers, #{geo_coord := GeoCoord} = Record) ->
    Lon = ogn_parser_tool:list_to_latlon(Value, LatLonParser),
    create_record(Tail, Parsers, Record#{geo_coord := GeoCoord#{longitude => Lon, lon => Value}});
create_record([{?ALTITUDE_KEY, Value} | Tail], Parsers, #{geo_coord := GeoCoord} = Record) ->
    Altitude = {list_to_integer(Value), foot},
    create_record(Tail, Parsers, Record#{geo_coord := GeoCoord#{altitude => Altitude}});
create_record([{?HEADING_KEY, Value} | Tail], Parsers, #{speed := Speed} = Record) ->
    Heading = {list_to_integer(Value), d},
    create_record(Tail, Parsers, Record#{speed := Speed#{heading => Heading}});
create_record([{?GROUND_SPEED_KEY, Value} | Tail], Parsers, #{speed := Speed} = Record) ->
    GroundSpeed = {list_to_integer(Value), kmph},
    create_record(Tail, Parsers, Record#{speed := Speed#{ground_speed => GroundSpeed}});
create_record([{?COMMENT_KEY, Value} | Tail], #{aircraft_comment := {CommentRegex, _}} = Parsers, Record) ->
    Parsed = re:run(Value, CommentRegex, ?RUN_OPTIONS),
    create_record(Tail, Parsers, parse_aircraft_comment(Parsed, Record));
create_record([_ | Tail], Parsers, Record) ->
    create_record(Tail, Parsers, Record).

%% @private
parse_aircraft_comment({match, [DeviceId, ClimbRate, TurnRate]}, #{speed := Speed} = Record) ->
    Record#{
        device_id => DeviceId,
        speed := Speed#{
            climb_rate => {list_to_integer(ClimbRate), footpmin}, 
            turn_rate => {list_to_float(TurnRate), tr}
        }    
    };
parse_aircraft_comment(nomatch, Record) ->
    Record.
