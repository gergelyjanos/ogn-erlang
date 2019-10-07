-module(db_mnesia_sup).

-behavior(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    ok = mnesia:create_schema([node()]),
    ok = mnesia:start(),
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init(_Args) ->
    RestartStrategy = #{
        strategy => one_for_one, 
        intensity => 1, 
        period => 5
    },
    Children = [
        db_mnesia_aircraftposition_config()
    ],
    {ok, {RestartStrategy, Children}}.

db_mnesia_aircraftposition_config() ->
    #{id => db_mnesia_aircraftposition_server,
        start => {db_mnesia_aircraftposition_server, start_link, []},
        restart => permanent,
        shutdown => brutal_kill,
        type => worker,
        modules => [db_mnesia_aircraftposition_server]}.

