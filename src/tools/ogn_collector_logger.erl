-module(ogn_collector_logger).

-export([info/2, debug/2, error/2]).

info(_Format, _Msg) ->
    ok.

debug(_Format, _Msg) ->
    ok.

error(_Format, _Msg) ->
    ok.

