-module(ogn_parser_line_parser).

-export([compile_parsers/0, parse_line/2]).

-define(RUN_OPTIONS, [{capture, all_names, list}]).
-define(COMPILE_OPTIONS, [unicode]).

-spec compile_parsers() -> Result
    when
        Result :: map().
compile_parsers()->
    #{
        aircraft_position => re_compile(aircraft_position),
        receiver_position => re_compile(receiver_position),
        receiver_status => re_compile(receiver_status),
        aircraft_comment => re_compile(aircraft_comment),
        timestamp => re_compile(timestamp),
        latlon => re_compile(latlon),
        ram => re_compile(ram)
    }.

-spec parse_line(Line, Parsers) -> Result
    when
        Line :: binary(),
        Parsers :: map(),
        Result :: nomatch | {atom(), any()}.
parse_line(Line, Parsers) ->
    parse_line_ordered(Line, Parsers, [aircraft_position, receiver_status, receiver_position]).

%% @private
parse_line_ordered(_, _, []) -> nomatch;
parse_line_ordered(Line, Parsers, [Pattern | Tail]) ->
    #{Pattern := {Regex, Namelist}} = Parsers,
    case re:run(Line, Regex, ?RUN_OPTIONS) of
        {match, DataList} ->
            LineValues = lists:zip(Namelist, DataList),
            DataRecord = create_result(Pattern, LineValues, Parsers),
            {Pattern, DataRecord};
        nomatch -> 
            parse_line_ordered(Line, Parsers, Tail)
    end.

%% @private
create_result(aircraft_position, LineValues, Parsers) ->
    ogn_parser_aircraft_position_parser:create_aircraft_position_record(LineValues, Parsers);
create_result(receiver_position, LineValues, Parsers) ->
    ogn_parser_receiver_position_parser:create_receiver_position_record(LineValues, Parsers);
create_result(receiver_status, LineValues, Parsers) ->
    ogn_parser_receiver_status_parser:create_receiver_status_record(LineValues, Parsers);
create_result(Pattern, LineValues, _OtherParsers) ->
    {Pattern, LineValues}.

%% @private
re_compile(Pattern) ->
    {ok, Regex} = re:compile(ogn_parser_tool:get_pattern(Pattern), ?COMPILE_OPTIONS),
    {namelist, NamelistBinary} = re:inspect(Regex, namelist),
    {Regex, NamelistBinary}.
