%% @author Janos Gergely <gergelyjanos@td1990.org>
%% @doc Module for HTTP server
%% @copyright 2019 TD*1990
%% @version 1.0.0

-module(http_server_server).
-behaviour(gen_server).

%% API
-export([start/0, stop/0, start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {listen}).

start() ->
   start_link().

stop() ->
   gen_server:call(?MODULE, stop).

start_link() ->
   gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
   Inet = inets:start(), % TODO error, already started?
   io:format("inet:start ~p~n", [Inet]),
   {ok, Pid} = inets:start(httpd, 
      [
         {modules, [ 
                  mod_esi
               ]
         },
         {port, 8080},
         % {bind_address, "localhost"},
         {server_name, "ogn"},
         {server_root, "."},
         {document_root, "."},
         {erl_script_alias, {"/", 
               [
                  web
               %   httpserver, 
               %   aircraftpositionapi, 
               %   receiverpositionapi,
               %   systeminfoapi
               ]}},
         {erl_script_nocache, true},
         {mime_types, [{"html","text/html"}, {"json", "application/json"}]}
      ]
   ),
   io:format("httpd ~p ~p~n", [Pid, httpd:info(Pid)]),
   {ok, #state{listen=ok}}.

handle_call(stop, _From, State) ->
   {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
   {noreply, State}.

handle_info(_Info, State) ->
   {noreply, State}.

terminate(_Reason, _State) ->
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.
