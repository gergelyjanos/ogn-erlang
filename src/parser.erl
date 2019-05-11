%% @author Janos Gergely <gergelyjanos@td1990.org>
%% @doc Function to parse APRS messages
%% @copyright 2019 TD*1990
%% @version 1.0.0

-module(parser).
-export([run/0]).

%% @doc Public function to parse messages.
run() ->
    receive 
        {line, Line} -> parseline(string:chomp(Line)), run();
        {comment, Comment} -> parsecomment(Comment), run();
        {close} -> io:format("Close parser~n")
    end
.	

%% @doc Private function to parse line.
parseline(Line) ->
    io:format("Line parser ~p~n", [Line]),
    {ok, PATTERN_APRS} = re:compile("^(?P<callsign>.+?)>(?P<dstcall>[A-Z0-9]+),((?P<relay>[A-Za-z0-9]+)\*)?.*,(?P<receiver>.+?):(?P<aprs_type>(/|>))(?P<aprs_body>.*)$"),
    case re:run(Line, PATTERN_APRS, [{capture, all_but_first, list}]) of 
        % {match, [A, Dstcall, Relay, Receiver, Aprs_type, Aprs_body]} ->
        %     io:format("Line parser match~n A\t~p~nDts\t~p~nRelay\t~p~n\t~p~n\t~p~n\t~p~n", [A, Dstcall, Relay, Receiver, Aprs_type, Aprs_body]),
        %     Position = parseposition(A),
        %     io:format("\t~p~n", [Position])
        % ;
        {match, [Callsign, Dstcall, A3, A4, Receiver, A6, A7, Body]} ->
            io:format("Line parser match~n ~p~n~p~n~p~n~p~n~p~n~p~n~p~n~p~n", [Callsign, Dstcall, A3, A4, Receiver, A6, A7, Body]),
            Position = parseposition(Body),
            io:format("position: \t~p~n", [Position])
        ;
        nomatch -> 
            io:format("Line parser nomatch~n")
    end
.	

%% @doc Private function to parse position.
parseposition(AprsBody) ->
    {ok, PATTERN_APRS_POSITION} = re:compile("^(?P<time>(([0-1]\d|2[0-3])[0-5]\d[0-5]\dh|([0-2]\d|3[0-1])([0-1]\d|2[0-3])[0-5]\dz))(?P<latitude>9000\.00|[0-8]\d{3}\.\d{2})(?P<latitude_sign>N|S)(?P<symbol_table>.)(?P<longitude>18000\.00|1[0-7]\d{3}\.\d{2}|0\d{4}\.\d{2})(?P<longitude_sign>E|W)(?P<symbol>.)(?P<course_extension>(?P<course>\d{3})/(?P<ground_speed>\d{3}))?/A=(?P<altitude>(-\d{5}|\d{6}))(?P<pos_extension>\s!W((?P<latitude_enhancement>\d)(?P<longitude_enhancement>\d))!)?(?:\s(?P<comment>.*))?$"),
    case re:run(AprsBody, PATTERN_APRS_POSITION, [{capture, all_but_first, list}]) of 
        {match, Datas} ->
            Datas;
        % {match, [Time, Latitude, Latitude_sign, Symbol_table, Longitude, Longitude_sign, Symbol, Course_extension, Course, Ground_speed, Altitude, Pos_extension, Latitude_enhancement, Longitude_enhancement, Comment]} ->
        %     [Time, Latitude, Latitude_sign, Symbol_table, Longitude, Longitude_sign, Symbol, Course_extension, Course, Ground_speed, Altitude, Pos_extension, Latitude_enhancement, Longitude_enhancement, Comment]
        % ;
        nomatch ->
            nomatch    
    end
.	


%% @doc Private function to parse comment.
parsecomment(Comment) ->
	io:format("Comment parser ~p~n", [Comment])
.	
