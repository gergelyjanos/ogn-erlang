-module(ogn_parser_multiplex_worker).
-behaviour(gen_server).

%% API
-export([start/0, start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%% api

%% gen server api

start() ->
   gen_server:start({local, ?SERVER}, ?MODULE, [], []).

start_link() ->
   gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init(_Args) ->
   Parsers = ogn_parser_line_parser:compile_parsers(),
   {ok, #{parsers => Parsers}}.

handle_call(stop, _From, State) ->
   {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({server_name, _ServerName}, State) ->
   % {ok, Pid} = ogn_parser_process_message_sup:start_parser_worker(),
   % gen_server:cast(Pid, {server_name, ServerName}),
   {noreply, State};
handle_cast({raw_line, Line}, #{parsers := Parsers} = State) ->
   ogn_parser_process_message_worker:parse_raw_line(Line, Parsers),
   {noreply, State}.

handle_info(_Info, State) ->
   {stop, normal, State}.

terminate(_Reason, _State) ->
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.
