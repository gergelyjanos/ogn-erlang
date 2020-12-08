-module(ogn_parser_receiver_mapper).

-export([create_receiver_record/2]).

-include("../ogn_collector.hrl").
-include("./ogn_parser_pattern.hrl").

-define(RUN_OPTIONS, [{capture, all_but_first, list}]).

create_receiver_record(LineValues, Parsers) ->
    create_record(LineValues, Parsers, #{geo_coord => #{}}).

%% @private
create_record([], _, Record) -> 
    Record;
create_record([{?RECEIVER_KEY, Value} | Tail], Parsers, Record) ->
    create_record(Tail, Parsers, Record#{receiver => Value, receiver_id => Value});
create_record([{?MESSAGE_FORMAT_KEY, Value} | Tail], Parsers, Record) ->
    create_record(Tail, Parsers, Record#{message_format => Value});
create_record([{?SERVER_KEY, Value} | Tail], Parsers, Record) ->
    create_record(Tail, Parsers, Record#{server => Value});
create_record([{?VERSION_KEY, Value} | Tail], Parsers, Record) ->
    create_record(Tail, Parsers, Record#{version => Value});
create_record([{?CPU_KEY, Value} | Tail], Parsers, Record) ->
    create_record(Tail, Parsers, Record#{cpu => Value});
create_record([{?OTHER_KEY, Value} | Tail], Parsers, Record) ->
    create_record(Tail, Parsers, Record#{other => Value});
create_record([{?TIMESTAMP_KEY, Value} | Tail], Parsers, Record) ->
    %% todo timestamp
    create_record(Tail, Parsers, Record#{timestamp => Value});
create_record([{?RAM_KEY, Value} | Tail], #{ram := {RamRegex, _}} = Parsers, Record) ->
    Parsed = re:run(Value, RamRegex, ?RUN_OPTIONS),
    create_record(Tail, Parsers, parse_ram(Parsed, Record));
create_record([{?LATITUDE_KEY, Value} | Tail], #{latlon := LatLonParser} = Parsers, #{geo_coord := GeoCoord} = Record) ->
    Lat = ogn_parser_tool:list_to_latlon(Value, LatLonParser),
    create_record(Tail, Parsers, Record#{geo_coord := GeoCoord#{latitude => Lat, lat => Value}});
create_record([{?LONGITUDE_KEY, Value} | Tail], #{latlon := LatLonParser} = Parsers, #{geo_coord := GeoCoord} = Record) ->
    Lon = ogn_parser_tool:list_to_latlon(Value, LatLonParser),
    create_record(Tail, Parsers, Record#{geo_coord := GeoCoord#{longitude => Lon, lon => Value}});
create_record([{?ALTITUDE_KEY, Value} | Tail], Parsers, #{geo_coord := GeoCoord} = Record) ->
    Altitude = {list_to_integer(Value), foot},
    create_record(Tail, Parsers, Record#{geo_coord := GeoCoord#{altitude => Altitude}});
create_record([_ | Tail], Parsers, Record) ->
    create_record(Tail, Parsers, Record).

%% @private
parse_ram({match, [UsedRam, AllRam, RamUnit]}, Record) ->
    Record#{ram => {RamUnit, list_to_float(UsedRam), list_to_float(AllRam)}};
parse_ram(nomatch, Record) ->
    Record.
