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


start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
    {{continue, connect}, #state{}}.

handle_continue({continue, connect}, State) ->
    case gen_tcp:connect(?AprsHost, ?AprsPort, ?PassivemodeConnectOptions, ?Timeout) of
		{ok, Socket} ->
			io:format("Socket connected: ~p~n", [Socket]),
            {{continue, login}, State#state{socket=Socket, line_count=0}};
		{error, _Reason} ->
            {{error, connect_error}, State}
	end;

handle_continue({continue, login}, State=#state{socket=Socket}) ->
    {ok, ServerName} = gen_tcp:recv(Socket, 0),
    io:format("Received ServerName: ~p~n", [ServerName]),
    ok = gen_tcp:send(Socket, ?LoginMessage),
    {{continue, run}, State};

handle_continue({continue, run}, #state{socket=Socket, line_count=LineCount}=State) ->
    case gen_tcp:recv(Socket, 0, ?Timeout) of
	    {ok, Line} -> 
            io:format("~p", [Line]),
			% Comment = string:prefix(Line, "#"),
			% if 
			% 	Comment == nomatch -> 
			% 		Parser ! {line, Line};
			% 	true -> 
			% 		sendkeepalive(Socket),
			% 		Parser ! {comment, Comment}
			% end,
            {{continue, run}, State#state{line_count = LineCount+1}};
    	{error, timeout} -> {};
		{error, closed} -> {};
		{error, _Reason} -> {}
    end.

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
