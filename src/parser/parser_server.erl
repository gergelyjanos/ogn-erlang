-module(parser_server).
-behaviour(gen_server).

%% API
-export([start/0, start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([parse_raw_line/1, parse_server_name/1]).

-record(state, {}).

%% api
-spec parse_raw_line(Line) -> Result
   when
      Line :: binary(),
      Result :: ok.

parse_raw_line(Line) ->
   {ok, Pid} = parser_sup:start_parser_server(),
   gen_server:cast(Pid, {raw_line, Line}),
   ok.

-spec parse_server_name(ServerName) -> Result
   when
      ServerName :: binary(),
      Result :: ok.
   
parse_server_name(ServerName) ->
   {ok, Pid} = parser_sup:start_parser_server(),
   gen_server:cast(Pid, {server_name, ServerName}),
   ok.

%% gen server api

start() ->
   gen_server:start(?MODULE, [], []).

start_link() ->
   gen_server:start_link(?MODULE, [], []).

init(_Args) ->
   {ok, #state{}}.

handle_call(stop, _From, State) ->
   {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({server_name, ServerName}, State) ->
   io:format("hanlde server name ~p~n", [ServerName]),
   {stop, normal, State};

handle_cast({raw_line, Line}, State) ->
   process_line(string:prefix(Line, "#"), Line),
   {stop, normal, State}.

handle_info(_Info, State) ->
   {stop, normal, State}.

terminate(_Reason, _State) ->
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.

%% parsers
-spec process_line(Comment, Line) -> ok
   when
      Comment :: nomatch | binary(),
      Line :: binary().

process_line(nomatch, Line) ->
   io:format("process line ~p~n", [Line]),
   ok;

process_line(Comment, _Line) ->
   io:format("process comment ~p~n", [Comment]),
   ok.

