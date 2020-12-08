%%%--------------------------------------------------------------------- 
%% @doc ogn_collector public API
%% @end
%%%--------------------------------------------------------------------- 

-module(ogn_collector_app).

-behavior(application).

-export([start/2, stop/1]).

-include("ogn_collector.hrl").

start(_Type, _Args) ->
    logger:set_primary_config(level, warning),
    ogn_collector_sup:start_link().

stop(_State) ->
    ok.
