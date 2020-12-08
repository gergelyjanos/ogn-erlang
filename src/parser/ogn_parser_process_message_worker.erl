-module(ogn_parser_process_message_worker).
-behaviour(gen_server).

%% API
-export([start/1, start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([parse_raw_line/2]).

-include("../ogn_collector.hrl").

%% api

parse_raw_line(<<"#", Comment/binary>>, Parsers) ->
   {ok, Pid} = ogn_parser_process_message_sup:start_parser_worker(Parsers),
   gen_server:cast(Pid, {comment, Comment}),
   ok;
parse_raw_line(Line, Parsers) ->
   {ok, Pid} = ogn_parser_process_message_sup:start_parser_worker(Parsers),
   gen_server:cast(Pid, {line, Line}),
   ok.

%% gen server api

start(Parsers) ->
   gen_server:start(?MODULE, [Parsers], []).

start_link() ->
   gen_server:start_link(?MODULE, [], []).

init([Parsers]) ->
   {ok, #{parsers => Parsers}}.

handle_call(stop, _From, State) ->
   {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({server_name, _ServerName}, State) ->
   {stop, normal, State};
handle_cast({line, Line}, #{parsers := Parsers} = State) ->
   process_parse_line_result(ogn_parser_line_parser:parse_line(Line, Parsers)),
   {stop, normal, State};
handle_cast({comment, Comment}, State) ->
   ?LOG_DEBUG("~p:cast comment ~p", [?MODULE, Comment]),
   {stop, normal, State}.

handle_info(_Info, State) ->
   {stop, normal, State}.

terminate(_Reason, _State) ->
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.

process_parse_line_result(nomatch) -> 
   ok;
process_parse_line_result({aircraft_position, Record}) ->
   ogn_repo:add_aircraft_position(Record),
   ok;
process_parse_line_result({receiver_position, Record}) ->
   ?LOG_DEBUG("--- RECEIVER POS ~p", [Record]),
   ok;
process_parse_line_result({receiver_status, Record}) ->
   ?LOG_DEBUG("+++ RECEIVER STA ~p", [Record]),
   ok. 
