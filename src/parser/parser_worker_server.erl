-module(parser_worker_server).
-behaviour(gen_server).

%% API
-export([start/0, start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([parse_raw_line/1]).

-record(state, {}).

%% api

-spec parse_raw_line(Line) -> Result
   when
      Line :: binary(),
      Result :: ok.
parse_raw_line(<<"#",Comment/binary>>) ->
   {ok, Pid} = parser_worker_sup:start_parser_worker(),
   gen_server:cast(Pid, {comment, Comment}), % todo add patterns
   ok;
parse_raw_line(Line) ->
   {ok, Pid} = parser_worker_sup:start_parser_worker(),
   gen_server:cast(Pid, {line, Line}), % todo add patterns
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

handle_cast({server_name, _ServerName}, State) ->
   {stop, normal, State};
handle_cast({line, _Line}, State) ->
   % io:format("~p:cast line ~p~n", [?MODULE, Line]),
   {stop, normal, State};
handle_cast({comment, Comment}, State) ->
   io:format("~p:cast comment ~p~n", [?MODULE, Comment]),
   {stop, normal, State}.

handle_info(_Info, State) ->
   {stop, normal, State}.

terminate(_Reason, _State) ->
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.
