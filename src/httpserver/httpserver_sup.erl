%%%--------------------------------------------------------------------- 
%%% TD*1990
%%%--------------------------------------------------------------------- 

-module(httpserver_sup).

-behavior(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link(httpserver_sup, []).

init(_Args) ->
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
    ChildSpecs = [#{id => httpserver,
                    start => {httpserver, start_link, []},
                    restart => permanent,
                    shutdown => brutal_kill,
                    type => worker,
                    modules => [httpserver]}],
    {ok, {SupFlags, ChildSpecs}}.
