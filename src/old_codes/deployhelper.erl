%% @author Janos Gergely <gergelyjanos@td1990.org>
%% @doc Module to compile and create docs and etc.
%% @copyright 2019 TD*1990
%% @version 1.0.0

-module(deployhelper).
-export([docs/0]).

docs() -> edoc:files(["deployhelper.erl", "startup.erl"], [{dir, "../doc"}]).
