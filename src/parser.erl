%% @author Janos Gergely <gergelyjanos@td1990.org>
%% @doc Function to parse APRS messages
%% @copyright 2019 TD*1990
%% @version 1.0.0

-module(parser).
-export([run/0]).

-define(beaconData, Device, MessageFormat, Receiver, Timestamp, Latitude, Longitude, Heading, GroundSpeed, Altitude, Comment).
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
    % {ok, PATTERN_APRS} = re:compile("^(?P<callsign>.+?)>(?P<dstcall>[A-Z0-9]+),((?P<relay>[A-Za-z0-9]+)\*)?.*,(?P<receiver>.+?):(?P<aprs_type>(/|>))(?P<aprs_body>.*)$"),
    {ok, PATTERN_Aircraft} = re:compile("^(?P<device>.+)>(?P<message_format>.+),.+,(?P<receiver>.+):\\/(?P<timestamp>\\d+)h(?P<latitude>\\d+\\.\\d+[NS])[\\/\\\\]+(?P<longitude>\\d+\\.\\d+[EW])['^](?P<heading>\\d+)\\/(?P<ground_speed>\\d+)\\/A=(?P<altitude>\\d+)\\s(?P<comment>.*)$", [unicode]),
    case re:run(Line, PATTERN_Aircraft, [{capture, all_but_first, list}]) of 
        {match, [?beaconData]} ->
            io:format("Line parser match ~p~n", [[?beaconData]])
        ;
        nomatch -> 
            io:format("Line parser nomatch ~p~n", [Line])
    end
.	

%% @doc Private function to parse comment.
parsecomment(Comment) ->
	io:format("Comment parser ~p~n", [Comment])
.	
