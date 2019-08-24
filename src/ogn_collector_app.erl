%%%--------------------------------------------------------------------- 
%% @doc ogn_collector public API
%% @end
%%%--------------------------------------------------------------------- 

-module(ogn_collector_app).

-behavior(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    ogn_collector_sup:start_link().

stop(_State) ->
    ok.
