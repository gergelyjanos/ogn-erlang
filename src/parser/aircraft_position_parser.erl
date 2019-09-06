-module(aircraft_position_parser).

-export([create_aircraft_position_record/2]).

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

-record(acc, {record, parsers}).

create_aircraft_position_record(Map, OtherParsers) ->
    #acc{record = Record} = lists:foldl(
        fun create_aircraft_position/2, 
        #acc{record = #aircraft_position{}, parsers = OtherParsers}, 
        Map),
    Record.

create_aircraft_position({device, Data}, #acc{record = Record} = Acc) ->
    Acc#acc{record = Record#aircraft_position{device = Data}};
create_aircraft_position({message_format, Data}, #acc{record = Record} = Acc) ->
    Acc#acc{record = Record#aircraft_position{message_format = Data}};
create_aircraft_position({receiver, Data}, #acc{record = Record} = Acc) ->
    Acc#acc{record = Record#aircraft_position{receiver = Data}};
create_aircraft_position({timestamp, Data}, #acc{record = Record} = Acc) ->
    Acc#acc{record = Record#aircraft_position{timestamp = Data}}; % todo parse
create_aircraft_position(_, Acc) ->
    Acc.
