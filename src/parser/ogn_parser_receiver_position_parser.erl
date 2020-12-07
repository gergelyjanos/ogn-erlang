-module(ogn_parser_receiver_position_parser).

-export([create_receiver_position_record/2]).

-include("./ogn_parser_pattern.hrl").

-define(RUN_OPTIONS, [{capture, all_but_first, list}]).

create_receiver_position_record(LineValues, Parsers) ->
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
create_record([{?ALTITUDE_KEY = Key, Value} | Tail], Parsers, Record) ->
    Altitude = {list_to_integer(Value), foot},
    create_record(Tail, Parsers, Record#{Key => Altitude});
create_record([{Key, Value} | Tail], Parsers, Record) ->
    create_record(Tail, Parsers, Record#{Key => Value}).
