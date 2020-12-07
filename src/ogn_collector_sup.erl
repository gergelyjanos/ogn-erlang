%%%--------------------------------------------------------------------- 
%% @doc ogn_collector top level supervisor.
%% @end
%%%--------------------------------------------------------------------- 

-module(ogn_collector_sup).

-behavior(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init(_Args) ->
    RestartStrategy = #{
        strategy => one_for_one, 
        intensity => 1, 
        period => 5
    },
    Children = [
        db_mnesia_sup_config(),
        ogn_parser_process_message_sup_config(),
        ogn_parser_multiplex_worker_config(),
        http_server_server_config(), 
        ogn_aprs_client_worker_config()
    ],
    {ok, {RestartStrategy, Children}}.

db_mnesia_sup_config()->
    #{
        id => db_mnesia_sup,
        start => {db_mnesia_sup, start_link, []},
        restart => permanent,
        shutdown => brutal_kill,
        type => supervisor,
        modules => [db_mnesia_sup]
    }.

http_server_server_config() ->
    #{
        id => http_server_server,
        start => {http_server_server, start_link, []},
        restart => permanent,
        shutdown => brutal_kill,
        type => worker,
        modules => [http_server_server]
    }.

ogn_aprs_client_worker_config() ->
    #{
        id => ogn_aprs_client_worker,
        start => {ogn_aprs_client_worker, start_link, []},
        restart => permanent,
        shutdown => brutal_kill,
        type => worker,
        modules => [ogn_aprs_client_worker]
    }.

ogn_parser_process_message_sup_config() ->
    #{
        id => ogn_parser_process_message_sup,
        start => {ogn_parser_process_message_sup, start_link, []},
        restart => permanent,
        shutdown => brutal_kill,
        type => supervisor,
        modules => [ogn_parser_process_message_sup]
    }.

ogn_parser_multiplex_worker_config() ->
    #{
        id => ogn_parser_multiplex_worker,
        start => {ogn_parser_multiplex_worker, start_link, []},
        restart => permanent,
        shutdown => brutal_kill,
        type => worker,
        modules => [ogn_parser_multiplex_worker]
    }.
