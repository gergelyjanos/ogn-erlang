-module(aprs_client_server).
-behaviour(gen_server).
% gen_statem, enter_loop, active mode socket

%% API
-export([start/0, stop/0, start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([handle_continue/2]).
-record(state, {dummy}).

start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
    {{continue, connect}, #state{dummy=1}}.

handle_continue({continue, connect}, State) ->
    {{continue, login}, State};
handle_continue({continue, login}, State) ->
    {{continue, run}, State};
handle_continue({continue, run}, State) ->
    {{continue, run}, State}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call(connect, _From, State) ->
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(login, State) ->
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
