-module(regex_parser).

-export([compile_parsers/0, parse_line/2]).

-define(RUN_OPTIONS, [{capture, all_names, list}]).
-define(COMPILE_OPTIONS, [unicode]).

compile_parsers()->
    { 
        [   {Key, re_compile(Key)} ||
            Key <- [
                aircraft_position, 
                receiver_position, 
                receiver_status]
        ],
        {
            {aircraft_comment, re_compile(aircraft_comment)},
            {timestamp, re_compile(timestamp)}
        }
    }.    

re_compile(Pattern) ->
    {ok, Regex} = re:compile(pattern:get_pattern(Pattern), ?COMPILE_OPTIONS),
    {namelist, Namelist} = re:inspect(Regex, namelist),
    {Regex, Namelist}.

parse_line(Line, {[{aircraft_position, Regex} | LineParsers], OtherParsers}) ->
    case parse_line(Line, aircraft_position, Regex) of
        nomatch ->
            parse_line(Line, {LineParsers, OtherParsers});
        Oke -> Oke
    end;
parse_line(Line, {[{receiver_position, Regex} | LineParsers], OtherParsers}) ->
    case parse_line(Line, receiver_position, Regex) of
        nomatch ->
            parse_line(Line, {LineParsers, OtherParsers});
        Oke -> Oke
    end;
parse_line(Line, {[{receiver_status, Regex} | LineParsers], OtherParsers}) ->
    case parse_line(Line, receiver_status, Regex) of
        nomatch ->
            parse_line(Line, {LineParsers, OtherParsers});
        Oke -> Oke
    end;
parse_line(Line, {[{_, _Regex} | LineParsers], OtherParsers}) ->
    parse_line(Line, {LineParsers, OtherParsers});
parse_line(_Line, {[], _OtherParsers}) ->
    nomatch.

parse_line(Line, Pattern, {Regex, Namelist}) ->
    case re_run(Line, Regex) of
        {match, DataList} ->
            Map = lists:zip(Namelist, DataList),
            {Pattern, Map};
        nomatch -> nomatch
    end.

re_run(Text, Regex) ->
    re:run(Text, Regex, ?RUN_OPTIONS).

