-module(parser_receiver_server).
-behaviour(gen_server).

-export([parse_raw_line/1, parse_server_name/1]).
-export([start/0, stop/0, start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {dummy}).

-define(SERVER, ?MODULE).

%% API
-spec parse_raw_line(Line) -> Result
   when
      Line :: binary(),
      Result :: term().

parse_raw_line(Line) ->
    gen_server:cast(?MODULE, {raw_line, Line}).

-spec parse_server_name(ServerName) -> Result
   when
      ServerName :: binary(),
      Result :: term().
   
parse_server_name(ServerName) ->
    gen_server:cast(?MODULE, {server_name, ServerName}).

%% gen_server API
start() ->
   gen_server:start({local, ?SERVER}, ?MODULE, [], []).

start_link() ->
   gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
   gen_server:call(?MODULE, stop).

init(_Args) ->
   {ok, #state{dummy=1}}.

handle_call(stop, _From, State) ->
   {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

-spec handle_cast({server_name, ServerName} | {raw_line, Line}, State) -> Result
   when
      ServerName :: binary(),
      Line :: binary(),
      State :: #state{},
      Result :: term().

handle_cast({server_name, ServerName}, State) ->
   io:format("parse server_name ~p~n", [ServerName]),
   {noreply, State};

handle_cast({raw_line, Line}, State) ->
   process_line(string:prefix(Line, "#"), Line),
   {noreply, State}.

handle_info(_Info, State) ->
   {noreply, State}.

terminate(_Reason, _State) ->
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.

-spec process_line(Comment, Line) -> ok
   when
      Comment :: nomatch | binary(),
      Line :: binary().

process_line(nomatch, Line) ->
   io:format("parse line ~p~n", [Line]),
   ok;
process_line(Comment, _Line) ->
   io:format("parse comment ~p~n", [Comment]),
   ok.



