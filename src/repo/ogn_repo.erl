-module(ogn_repo).

-export([
    update_aircraft/1,
    update_receiver/1,
    info/0
    % add_receiver_position/1,
    % add_receiver_status/1,
    % get_aircraft_last_position/1,
    % get_receiver_last_position/1,
    % list_aircraft_position/2,
    % list_aircrafts_live_position/0,
    % list_receivers_live_position/0
]).

-include("../ogn_collector.hrl").


update_aircraft(Record) ->
    gen_server:cast(ogn_repo_map_worker, {update_aircraft, Record}),
    % ?LOG_DEBUG("UPDATE AIRCRAFT ~p", [Record]),
    ok.

update_receiver(Record) ->
    gen_server:cast(ogn_repo_map_worker, {update_receiver, Record}),
    % ?LOG_DEBUG("UPDATE RECEIVER ~p", [Record]),
    ok.

info() ->
    Size = gen_server:call(ogn_repo_map_worker, db_size),
    ?LOG_INFO("REPO SIZE ~p", [Size]),
    Size.


% modify_receiver_position(_Position) ->
%     ok.

% modify_receiver_status(_Position) ->
%     ok.

% get_receiver_last_position(_Receiver) ->
%     not_found.

% get_aircraft_last_position(_Aircraft) ->
%     not_found.

% list_aircraft_position_history(_Aircraft, _Interval) ->
%     [].

% list_aircrafts_live_position() ->
%     [].

% list_receivers_live_position() ->
%     [].

