-module(parser_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-export([start_parser_server/0]).

-define(SERVER, ?MODULE).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

start_parser_server() ->
    supervisor:start_child(?MODULE, []).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init(_Args) ->
    RestartStrategy = #{
        strategy => simple_one_for_one, 
        intensity => 1, 
        period => 5
    },
    Children = [
        parser_server_config()
    ],
    {ok, {RestartStrategy, Children}}.

parser_server_config() ->
    #{id => parser_server,
        start => {parser_server, start, []},
        restart => transient,
        shutdown => brutal_kill,
        type => worker,
        modules => [parser_server]}.
