-module(aprs_client_server).
-behaviour(gen_server).

%% API
-export([start/0, stop/0, start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([handle_continue/2]).

-record(state, {socket, line_count :: integer(), keepalive_time :: integer()}).

-define(AprsPort, 10152).
-define(AprsHost, "aprs.glidernet.org").
-define(PassivemodeConnectOptions,	[binary, {active, false}, {packet, line}, {delay_send, false}, {nodelay, true}]).
-define(RECVTIMEOUT, 20000).
-define(CONNECTTIMEOUT, 20000).
-define(LoginMessage, "user td1990 pass -1 vers ogn_erlang 1.0.0\r\n").

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
        Result :: term().
init(_Args) ->
    {ok, #state{}, {continue, connect}}.

-spec handle_continue(connect | login | run, State) -> Result
    when
        State :: #state{},
        Result :: term().

handle_continue(connect, State) ->
    process_connect(gen_tcp:connect(?AprsHost, ?AprsPort, ?PassivemodeConnectOptions, ?CONNECTTIMEOUT), State);

handle_continue(login, State=#state{socket=Socket}) ->
    {ok, ServerName} = gen_tcp:recv(Socket, 0, ?RECVTIMEOUT),
    parser_server:parse_server_name(ServerName),
    ok = gen_tcp:send(Socket, ?LoginMessage),
    {noreply, State#state{keepalive_time = erlang:system_time(second)}, {continue, run}};

handle_continue(run, #state{socket=Socket}=State) ->
    process_recv(gen_tcp:recv(Socket, 0, ?RECVTIMEOUT), State).

-spec handle_call(stop, _From, State) -> Result
    when
        _From :: term(),
        State :: #state{},
        Result :: term().

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State}.

-spec handle_cast(_Msg, State) -> Result   
    when
        _Msg :: term(),
        State :: #state{},
        Result :: term().

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(_Info, State) -> Result   
    when
        _Info :: term(),
        State :: #state{},
        Result :: term().
    
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(_Reason, _State) -> Result   
    when
        _Reason :: term(),
        _State :: #state{},
        Result :: term().
    
terminate(_Reason, _State) ->
    ok.

-spec code_change(_OldVsn, State, _Extra) -> Result   
    when
        _OldVsn :: term(),
        State :: #state{},
        _Extra :: term(),
        Result :: term().
    
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% private

-spec process_recv(
    {ok, Line} | {error, timeout} | {error, closed} | {error, Reason},
    State) -> Result
    when
        Line :: binary(),
        Reason :: term(),
        State :: #state{},
        Result :: term().

process_recv({ok, Line}, #state{line_count=LineCount}=State) -> 
    parser_server:parse_raw_line(Line),
    process_keepalive(State#state{line_count = LineCount+1}, erlang:system_time(second));

process_recv({error, timeout}, #state{}=State) -> 
    send_keepalive(State);

process_recv({error, closed}, State) -> 
    {stop, closed, State};

process_recv({error, Reason}, State) -> 
    {stop, Reason, State}.

-spec send_keepalive(State) -> Result
    when
        State :: #state{},
        Result :: term().

send_keepalive(#state{socket=Socket}=State) ->
    gen_tcp:send(Socket, "#keepalive"),
    {noreply, State#state{keepalive_time = erlang:system_time(second)}, {continue, run}}.

-spec process_keepalive(State, SystemTime) -> Result
    when
        State :: #state{},
        SystemTime :: integer(),
        Result :: term().

process_keepalive(#state{keepalive_time=KeepAliveTime}=State, SystemTime)
    when SystemTime - ?RECVTIMEOUT > KeepAliveTime ->
        send_keepalive(State);

process_keepalive(#state{}=State, _SystemTime) ->
    {noreply, State, {continue, run}}.

-spec process_connect({ok, Socket} | {error, Reason}, State) -> Result
    when
        Socket :: term(),
        Reason :: term(),
        State :: #state{},
        Result :: term().

process_connect({ok, Socket}, #state{}=State) ->
    % io:format("Socket connected: ~p~n", [Socket]),
    {noreply, State#state{socket=Socket, line_count=0}, {continue, login}};

process_connect({error, Reason}, State) ->
    {stop, Reason, State}.
