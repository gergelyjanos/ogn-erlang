-module(ogn_repo_sup).

-behavior(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init(_Args) ->
    RestartStrategy = #{
        strategy => one_for_one, 
        intensity => 1, 
        period => 5
    },
    Children = [
        map_worker_config()
    ],
    {ok, {RestartStrategy, Children}}.

map_worker_config() ->
    #{
        id => ogn_repo_map_worker,
        start => {ogn_repo_map_worker, start_link, []},
        restart => permanent,
        shutdown => brutal_kill,
        type => worker,
        modules => [ogn_repo_map_worker]
    }.

% db_mnesia_aircraftposition_config() ->
%     #{id => db_mnesia_aircraftposition_server,
%         start => {db_mnesia_aircraftposition_server, start_link, []},
%         restart => permanent,
%         shutdown => brutal_kill,
%         type => worker,
%         modules => [db_mnesia_aircraftposition_server]}.

