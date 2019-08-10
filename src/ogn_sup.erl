%%%--------------------------------------------------------------------- 
%%% TD*1990
%%%--------------------------------------------------------------------- 

-module(ogn_sup).

-behavior(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link(?MODULE, []).

init(_Args) ->
    HttpServerServer = #{id => http_server_server,
                    start => {http_server_server, start_link, []},
                    restart => permanent,
                    shutdown => brutal_kill,
                    type => worker,
                    modules => [http_server_server]},

    RestartStrategy = #{strategy => one_for_one, intensity => 1, period => 5},
    Children = [HttpServerServer],
    {ok, {RestartStrategy, Children}}.
