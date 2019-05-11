%% @author Janos Gergely <gergelyjanos@td1990.org>
%% @doc Function to parse APRS messages
%% @copyright 2019 TD*1990
%% @version 1.0.0

-module(parser).
-export([run/0]).

%% @doc Public function to parse messages.
run() ->
    receive 
        {line, Line} -> parseline(Line), run();
        {comment, Comment} -> parsecomment(Comment), run();
        {close} -> io:format("Close parser~n")
    end
.	

%% @doc Public function to parse line.
parseline(Line) ->
    io:format("Line parser ~p~n", [Line])
.	

%% @doc Public function to parse comment.
parsecomment(Comment) ->
	io:format("Comment parser ~p~n", [Comment])
.	
