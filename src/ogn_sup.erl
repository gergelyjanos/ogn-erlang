%%%--------------------------------------------------------------------- 
%%% TD*1990
%%%--------------------------------------------------------------------- 

-module(ogn_sup).
-behavior(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    RestartStrategy = #{strategy => one_for_one, intensity => 1, period => 5},
    Children = [http_server_server_config()],
    {ok, {RestartStrategy, Children}}.

http_server_server_config() ->
    #{id => http_server_server,
        start => {http_server_server, start_link, []},
        restart => permanent,
        shutdown => brutal_kill,
        type => worker,
        modules => [http_server_server, aprs_client_server_config()]}.

aprs_client_server_config() ->
    #{id => aprs_client_server,
        start => {aprs_client_server, start_link, []},
        restart => permanent,
        shutdown => brutal_kill,
        type => worker,
        modules => [aprs_client_server]}.


