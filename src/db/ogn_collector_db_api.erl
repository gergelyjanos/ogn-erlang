-module(ogn_collector_db_api).

-export([
    add_aircraft_position/1,
    modify_receiver_position/1,
    modify_receiver_status/1,
    get_aircraft_last_position/1,
    get_receiver_last_position/1,
    list_aircraft_position_history/2,
    list_aircrafts_live_position/0,
    list_receivers_live_position/0
]).

add_aircraft_position(_Position) ->
    ok.

modify_receiver_position(_Position) ->
    ok.

modify_receiver_status(_Position) ->
    ok.

get_receiver_last_position(_Receiver) ->
    not_found.

get_aircraft_last_position(_Aircraft) ->
    not_found.

list_aircraft_position_history(_Aircraft, _Interval) ->
    [].

list_aircrafts_live_position() ->
    [].

list_receivers_live_position() ->
    [].

