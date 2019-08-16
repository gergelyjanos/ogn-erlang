-module(aprs_client_server).
-behaviour(gen_server).
% gen_statem, enter_loop, active mode socket

%% API
-export([start/0, stop/0, start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([handle_continue/2]).
-record(state, {socket, line_count}).

-define(AprsPort, 10152).
-define(AprsHost, "aprs.glidernet.org").
-define(PassivemodeConnectOptions,	[binary, {active, false}, {packet, line}, {delay_send, false}, {nodelay, true}]).
-define(Timeout, 20000).
-define(LoginMessage, "user td1990 pass -1 vers ogn_erlang 1.0.0\r\n").

-define(SERVER, ?MODULE).

start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE).

init(_Args) ->
    {ok, #state{}, {continue, connect}}.

handle_continue(connect, State) ->
    process_connect(gen_tcp:connect(?AprsHost, ?AprsPort, ?PassivemodeConnectOptions, ?Timeout), State);

handle_continue(login, State=#state{socket=Socket}) ->
    {ok, ServerName} = gen_tcp:recv(Socket, 0),
    parser_receiver_server:parse_server_name(ServerName),
    ok = gen_tcp:send(Socket, ?LoginMessage),
    {noreply, State, {continue, run}};

handle_continue(run, #state{socket=Socket}=State) ->
    process_recv(gen_tcp:recv(Socket, 0, ?Timeout), State).

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

%%% private

process_line(nomatch, Line, _Socket) ->
    % io:format("~p~n", [Line]),
    parser_receiver_server:parse_raw_line(Line),
    {ok};

process_line(Comment, _Line, Socket) ->
    % io:format("---> # ~p~n", [Comment]),
    gen_tcp:send(Socket, "#keepalive"),
    parser_receiver_server:parse_comment(Comment),
    {ok}.

process_recv({ok, Line}, #state{socket=Socket, line_count=LineCount}=State) -> 
    process_line(string:prefix(Line, "#"), Line, Socket),
    {noreply, State#state{line_count = LineCount+1}, {continue, run}};

process_recv({error, timeout}, State) -> 
    {stop, timeout, State};

process_recv({error, closed}, State) -> 
    {stop, closed, State};

process_recv({error, Reason}, State) -> 
    {stop, Reason, State}.

process_connect({ok, Socket}, #state{}=State) ->
    % io:format("Socket connected: ~p~n", [Socket]),
    {noreply, State#state{socket=Socket, line_count=0}, {continue, login}};

process_connect({error, Reason}, State) ->
    {stop, Reason, State}.
