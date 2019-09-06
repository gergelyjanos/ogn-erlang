-module(line_parser).

-export([compile_parsers/0, parse_line/2]).

-define(RUN_OPTIONS, [{capture, all_names, list}]).
-define(COMPILE_OPTIONS, [unicode]).

-record(parser_acc, {pattern, line, other_parsers, data_record}).

compile_parsers()->
    { 
        [   {Pattern, re_compile(Pattern)} ||
            Pattern <- [
                aircraft_position, 
                receiver_position, 
                receiver_status]
        ],
        #{
            aircraft_comment => re_compile(aircraft_comment),
            timestamp => re_compile(timestamp),
            latlon => re_compile(latlon)
        }
    }.    

re_compile(Pattern) ->
    {ok, Regex} = re:compile(pattern:get_pattern(Pattern), ?COMPILE_OPTIONS),
    {namelist, NamelistBinary} = re:inspect(Regex, namelist),
    Namelist = [binary_to_atom(Name, utf8) || Name <- NamelistBinary],
    {Regex, Namelist}.

parse_line(Line, {LineParsers, OtherParsers}) ->
    #parser_acc{pattern = Pattern, data_record = DataRecord} = lists:foldl(
        fun process_parse_line/2, 
        #parser_acc{pattern = nomatch, line = Line, other_parsers=OtherParsers}, 
        LineParsers),
    {Pattern, DataRecord}.

process_parse_line(
    {Pattern, {Regex, Namelist}}, 
    #parser_acc{pattern = nomatch, line = Line, other_parsers = OtherParsers} = Acc) ->
    case re:run(Line, Regex, ?RUN_OPTIONS) of
        {match, DataList} ->
            Map = lists:zip(Namelist, DataList),
            DataRecord = create_result(Pattern, Map, OtherParsers),
            Acc#parser_acc{pattern = Pattern, data_record = DataRecord};
        nomatch -> 
            Acc
    end;
process_parse_line(_, Acc) ->
    Acc.

create_result(aircraft_position, Map, OtherParsers) ->
    aircraft_position_parser:create_aircraft_position_record(Map, OtherParsers).

