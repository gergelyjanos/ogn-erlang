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
create_aircraft_position({latitude, Data}, #acc{record = Record, parsers = Parsers} = Acc) ->
    Lat = parser_tool:list_to_latlon(Data, maps:get(latlon, Parsers)),
    Acc#acc{record = Record#aircraft_position{latitude = Lat}};
create_aircraft_position({longitude, Data}, #acc{record = Record, parsers = Parsers} = Acc) ->
    Lon = parser_tool:list_to_latlon(Data, maps:get(latlon, Parsers)),
    Acc#acc{record = Record#aircraft_position{longitude = Lon}}; % todo parse
create_aircraft_position({heading, Data}, #acc{record = Record} = Acc) ->
    Acc#acc{record = Record#aircraft_position{heading = Data}}; % todo parse
create_aircraft_position({ground_speed, Data}, #acc{record = Record} = Acc) ->
    Acc#acc{record = Record#aircraft_position{ground_speed = Data}}; % todo parse
create_aircraft_position({altitude, Data}, #acc{record = Record} = Acc) ->
    Acc#acc{record = Record#aircraft_position{altitude = Data}}; % todo parse
% create_aircraft_position({comment, Data}, #acc{record = Record} = Acc) ->
%     Acc#acc{record = Record#aircraft_position{altitude = Data}}; % todo parse
create_aircraft_position(_, Acc) ->
    Acc.
