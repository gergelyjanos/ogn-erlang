-module(ogn_collector_aircraft_position).

-export([
    new/0
    , set_value/3
    , set_values/2
    % , set_device/2
    % , set_message_format/2
    % , set_receiver/2
]).
-export_type([aircraft_position/0]).

-record(aircraft_position, 
{
    device, 
    message_format, 
    receiver, 
    timestamp, 
    latitude, 
    longitude, 
    heading, 
    ground_speed, 
    altitude, 
    device_id, 
    climb_rate, 
    turn_rate
}).

-opaque aircraft_position() :: #aircraft_position{}.

new() ->
    #aircraft_position{}.

set_values(#aircraft_position{} = Rec, [{Key, Value} | Tail]) ->
    Rec2 = set_value(Rec, Key, Value),
    set_values(Rec2, Tail);
set_values(#aircraft_position{} = Rec, []) ->
    Rec.

set_value(#aircraft_position{} = Rec, device, Value) ->
    Rec#aircraft_position{device = Value};
set_value(#aircraft_position{} = Rec, message_format, Value) ->
    Rec#aircraft_position{message_format = Value};
set_value(#aircraft_position{} = Rec, receiver, Value) ->
    Rec#aircraft_position{receiver = Value};
set_value(#aircraft_position{} = Rec, timestamp, Value) ->
    Rec#aircraft_position{timestamp = Value};
set_value(#aircraft_position{} = Rec, latitude, Value) ->
    Rec#aircraft_position{latitude = Value};
set_value(#aircraft_position{} = Rec, longitude, Value) ->
    Rec#aircraft_position{longitude = Value};
set_value(#aircraft_position{} = Rec, heading, Value) ->
    Rec#aircraft_position{heading = Value};
set_value(#aircraft_position{} = Rec, ground_speed, Value) ->
    Rec#aircraft_position{ground_speed = Value};
set_value(#aircraft_position{} = Rec, altitude, Value) ->
    Rec#aircraft_position{altitude = Value};
set_value(#aircraft_position{} = Rec, device_id, Value) ->
    Rec#aircraft_position{device_id = Value};
set_value(#aircraft_position{} = Rec, climb_rate, Value) ->
    Rec#aircraft_position{climb_rate = Value};
set_value(#aircraft_position{} = Rec, turn_rate, Value) ->
    Rec#aircraft_position{turn_rate = Value}.

