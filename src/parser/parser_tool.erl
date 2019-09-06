-module(parser_tool).

-export([list_to_latlon/2, parse_aircraft_comment/2]).

-define(RUN_OPTIONS, [{capture, all_but_first, list}]).

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

parse_aircraft_comment(Data, {Regex, _}) ->
    case re:run(Data, Regex, ?RUN_OPTIONS) of
        {match, [DeviceId, ClimbRate, TurnRate]} ->
            {DeviceId, {list_to_integer(ClimbRate), footpmin}, {list_to_float(TurnRate), tr}};
        nomatch -> 
            {"", {0.0, footpmin}, {0.0, tr}}
    end.
