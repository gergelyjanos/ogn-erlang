-module(ogn_parser_process_message_worker).
-behaviour(gen_server).

%% API
-export([start/1, start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([parse_raw_line/2]).

-include("../ogn_collector.hrl").
-include("ogn_parser_pattern.hrl").


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
   Record1 = ogn_parser_line_parser:parse_line(Line, Parsers),
   Record2 = remove_empty_maps(Record1),
   process_parse_line_result(Record2, Line),
   {stop, normal, State};
handle_cast({comment, Comment}, State) ->
   ?LOG_WARNING("comment ~p", [Comment]),
   {stop, normal, State}.

handle_info(_Info, State) ->
   {stop, normal, State}.

terminate(_Reason, _State) ->
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.

process_parse_line_result(nomatch, Line) -> 
   ?LOG_ERROR("NOMATCH ~p", [Line]),
   ok;
process_parse_line_result({aircraft_position, Record}, _Line) ->
   ogn_repo:update_aircraft(Record),
   ok;
process_parse_line_result({receiver_position, Record}, _Line) ->
   ogn_repo:update_receiver(Record),
   ok;
process_parse_line_result({receiver_status, Record}, _Line) ->
   ogn_repo:update_receiver(Record),
   ok;
process_parse_line_result({receiver_position_status, Record}, _Line) ->
   ogn_repo:update_receiver(Record),
   ok;
process_parse_line_result(Res, Line) -> 
   ?LOG_ERROR("ERROR ~p ~p", [Res, Line]),
   ok.

%% @private
remove_empty_maps(#{geo_coord := GeoCoord} = Record) when map_size(GeoCoord) == 0 ->
    remove_empty_maps(maps:remove(geo_coord, Record));
remove_empty_maps(#{speed := Speed} = Record) when map_size(Speed) == 0 ->
    remove_empty_maps(maps:remove(speed, Record));
remove_empty_maps(Record) ->
    Record.
