-module(ogn_parser_tool).

-export([list_to_latlon/2]).
-export([get_pattern/1]).

-include("./ogn_parser_pattern.hrl").

-define(RUN_OPTIONS, [{capture, all_but_first, list}]).

get_pattern(aircraft_position) -> 
    ?AIRCRAFT_POSITION_PATTERN;
get_pattern(aircraft_comment) ->
    ?AIRCRAFTCOMMENT_PATTERN;
get_pattern(receiver_position) ->
    ?RECEIVERPOSITION_PATTERN;
get_pattern(receiver_status) ->
    ?RECEIVERSTATUS_PATTERN;
get_pattern(timestamp) ->
    ?TIMESTAMP_PATTERN;
get_pattern(latlon) ->
    ?LATLON_PATTERN.

list_to_latlon(Text, {Regex, _Namelist}) ->
    case re:run(Text, Regex, ?RUN_OPTIONS) of
        {match, Data} ->
            latlontext_to_number(Data);
        nomatch -> 
            {0.0, degree}
    end.

latlontext_to_number([Degree, Minute, Second, Globe]) ->
    DD = 
        list_to_integer(Degree) + 
        (list_to_integer(Minute) / 60) + 
        (list_to_integer(Second) / 3600),
    case Globe of
        "N" ->
            {DD, degree};
        "S" ->
            {-DD, degree};
        "E" ->
            {DD, degree};
        "W" ->
            {-DD, degree}
    end.

