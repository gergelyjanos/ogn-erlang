%%%--------------------------------------------------------------------- 
%%% TD*1990
%%%--------------------------------------------------------------------- 

-module(ogn_sup).

-behavior(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link(ogn_sup, []).

init(_Args) ->
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
    ChildSpecs = [
        #{id => httpserver_sup,
            start => {httpserver_sup, start_link, []},
            restart => permanent,
            shutdown => brutal_kill,
            type => supervisor,
            modules => [httpserver_sup]}
        ],
    {ok, {SupFlags, ChildSpecs}}.
