-module(ogn_parser_process_message_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-export([start_parser_worker/1]).

-define(SERVER, ?MODULE).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

start_parser_worker(Parsers) ->
    supervisor:start_child(?MODULE, [Parsers]).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init(_Args) ->
    RestartStrategy = #{
        strategy => simple_one_for_one, 
        intensity => 1, 
        period => 5
    },
    Children = [
        ogn_parser_process_message_worker_config()
    ],
    {ok, {RestartStrategy, Children}}.

ogn_parser_process_message_worker_config() ->
    #{id => ogn_parser_process_message_worker,
        start => {ogn_parser_process_message_worker, start, []},
        restart => transient,
        shutdown => brutal_kill,
        type => worker,
        modules => [ogn_parser_process_message_worker]}.

