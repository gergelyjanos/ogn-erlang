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
        parser_worker_sup_config(),
        parser_server_config(),
        http_server_server_config(), 
        aprs_client_server_config()
    ],
    {ok, {RestartStrategy, Children}}.

http_server_server_config() ->
    #{id => http_server_server,
        start => {http_server_server, start_link, []},
        restart => permanent,
        shutdown => brutal_kill,
        type => worker,
        modules => [http_server_server]}.

aprs_client_server_config() ->
    #{id => aprs_client_server,
        start => {aprs_client_server, start_link, []},
        restart => permanent,
        shutdown => brutal_kill,
        type => worker,
        modules => [aprs_client_server]}.

parser_worker_sup_config() ->
    #{id => parser_worker_sup,
        start => {parser_worker_sup, start_link, []},
        restart => permanent,
        shutdown => brutal_kill,
        type => supervisor,
        modules => [parser_worker_sup]}.

parser_server_config() ->
    #{id => parser_server,
        start => {parser_server, start_link, []},
        restart => permanent,
        shutdown => brutal_kill,
        type => worker,
        modules => [parser_server]}.
