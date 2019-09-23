-module(ogn_collector_logger).

-export([info/2, debug/2, error/2]).

info(Format, Msg) ->
    io:format(Format, Msg),
    ok.

debug(Format, Msg) ->
    io:format(Format, Msg),
    ok.

error(Format, Msg) ->
    io:format(Format, Msg),
    ok.

