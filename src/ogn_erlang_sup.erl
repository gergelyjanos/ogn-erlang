%%%--------------------------------------------------------------------- 
%% @doc ogn_erlang top level supervisor.
%% @end
%%%--------------------------------------------------------------------- 

-module(ogn_erlang_sup).

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
        http_server_server_config(), 
        aprs_client_server_config(),
        parser_sup_config()
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

parser_sup_config() ->
    #{id => parser_sup,
        start => {parser_sup, start_link, []},
        restart => permanent,
        shutdown => brutal_kill,
        type => supervisor,
        modules => [parser_sup]}.
