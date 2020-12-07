-module(ogn_aprs_client_worker).
-behaviour(gen_server).

%% API
-export([start/0, stop/0, start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([handle_continue/2]).

-include("../ogn_collector.hrl").

% -define(APRS_PORT, 10152).
% -define(APRS_HOST, "aprs.glidernet.org").
-define(PASSIVE_MODE_CONNECTION_OPTIONS,	[binary, {active, false}, {packet, line}, {delay_send, false}, {nodelay, true}]).
-define(RECV_TIMEOUT, 20000).
-define(KEEPALIVE_TIMEOUT, 200).
-define(CONNECT_TIMEOUT, 20000).
% -define(LOGIN_MESSAGE, "user td1990 pass -1 vers ogn_erlang 1.0.0\r\n").

-define(SERVER, ?MODULE).

-spec start() -> Result
    when
        Result :: term().
start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

-spec start_link() -> Result
    when
        Result :: term().
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec stop() -> Result
    when
        Result :: term().
stop() ->
    gen_server:call(?MODULE).

-spec init(_Args) -> Result
    when
        _Args :: term(),
        Result :: {ok, #{}, {continue, connect}}.
init(_Args) ->
    next_state(connect, #{}).

-spec handle_continue(connect | login | run, State) -> Result
    when
        State :: #{},
        Result :: term().
handle_continue(connect, State) ->
    {ok, Socket} = gen_tcp:connect(
        get_env(aprs_host), %% ?APRS_HOST, 
        get_env(aprs_port), %% ?APRS_PORT, 
        ?PASSIVE_MODE_CONNECTION_OPTIONS, 
        ?CONNECT_TIMEOUT), 
    next_state(login, State#{socket => Socket, line_count => 0});
handle_continue(login, #{socket := Socket} = State) ->
    {ok, ServerName} = gen_tcp:recv(Socket, 0, ?RECV_TIMEOUT),
    ogn_parser:parse_server_name(ServerName),
    ok = gen_tcp:send(Socket, get_env(aprs_login_message)), %% ?LOGIN_MESSAGE),
    next_state(run, set_keepalive(State));
handle_continue(run, #{socket := Socket} = State) ->
    process_recv(gen_tcp:recv(Socket, 0, ?RECV_TIMEOUT), State).

-spec handle_call(stop, _From, State) -> Result
    when
        _From :: term(),
        State :: #{},
        Result :: {stop, normal, stopped, State}.
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State}.

-spec handle_cast(_Msg, State) -> Result   
    when
        _Msg :: term(),
        State :: #{},
        Result :: {noreply, State}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(_Info, State) -> Result   
    when
        _Info :: term(),
        State :: #{},
        Result :: term().
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(_Reason, _State) -> Result   
    when
        _Reason :: term(),
        _State :: #{},
        Result :: term().
terminate(_Reason, _State) ->
    ok.

-spec code_change(_OldVsn, State, _Extra) -> Result   
    when
        _OldVsn :: term(),
        State :: #{},
        _Extra :: term(),
        Result :: term().
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @private
process_recv({ok, Line}, #{line_count := LineCount} = State) -> 
    ogn_parser:parse_raw_line(Line),
    send_keepalive(
        State#{line_count => LineCount + 1}, 
        erlang:system_time(second));
process_recv({error, timeout}, #{} = State) -> 
    send_keepalive(State, 0);
process_recv({error, closed}, State) -> 
    next_state(closed, State);
process_recv({error, Reason}, State) -> 
    next_state(stop, Reason, State).

%% @private
send_keepalive(#{keepalive_time := KeepAliveTime, socket := Socket} = State, SystemTime)
    when (SystemTime == 0) orelse (SystemTime - ?KEEPALIVE_TIMEOUT > KeepAliveTime) ->
        ?LOG_INFO("send #keepalive"),
        gen_tcp:send(Socket, "#keepalive"),
        next_state(run, set_keepalive(State));
send_keepalive(State, _SystemTime) ->
    next_state(run, State).

%% @private
set_keepalive(State) ->
    State#{keepalive_time => erlang:system_time(second)}.

%% @private
get_env(Key) ->
    {ok, Val} = application:get_env(Key),
    Val.

%% @private
next_state(connect, State) ->
    {ok, State, {continue, connect}};
next_state(closed, State) ->
    {stop, closed, State};
next_state(run, State) ->
    {noreply, State, {continue, run}};
next_state(login, State) ->
    {noreply, State, {continue, login}}.
next_state(stop, Reason, State) ->
    {stop, Reason, State}.

