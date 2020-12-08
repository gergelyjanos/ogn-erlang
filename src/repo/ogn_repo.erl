-module(ogn_repo).

-export([
    update_aircraft/1,
    update_receiver/1,
    info/0,
    get_aircraft/1,
    get_receiver/1,
    list_aircraft_ids/0,
    list_receiver_ids/0,
    list_aircrafts/0,
    list_receivers/0
]).

-include("../ogn_collector.hrl").

update_aircraft(Record) ->
    cast({update_aircraft, Record}),
    % ?LOG_DEBUG("UPDATE AIRCRAFT ~p", [Record]),
    ok.

update_receiver(Record) ->
    cast({update_receiver, Record}),
    % ?LOG_DEBUG("UPDATE RECEIVER ~p", [Record]),
    ok.

info() ->
    [
        {aircrafts, call({list_aircrafts})},
        {receivers, call({list_receivers})},
        {db_size, call(db_size)}
    ].

get_aircraft(Id) ->
    call({get_aircraft, Id}).

get_receiver(Id) ->
    call({get_receiver, Id}).

list_aircraft_ids() ->
    call({list_aircraft_ids}).

list_receiver_ids() ->
    call({list_receiver_ids}).

list_aircrafts() ->
    call({list_aircrafts}).

list_receivers() ->
    call({list_receivers}).

%% @private
call(Args) ->
    gen_server:call(ogn_repo_map_worker, Args).

cast(Args) ->
    gen_server:cast(ogn_repo_map_worker, Args).
