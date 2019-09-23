-module(aircraft_position_parser).

-export([create_aircraft_position_record/2]).

-record(acc, {record :: ogn_collector_aircraft_position:aircraft_position(), parsers}).

create_aircraft_position_record(Map, OtherParsers) ->
    #acc{record = Record} = lists:foldl(
        fun create_aircraft_position/2, 
        #acc{record = ogn_collector_aircraft_position:new(), parsers = OtherParsers}, 
        Map),
    Record.

create_aircraft_position({device, Data}, #acc{record = Record} = Acc) ->
    Acc#acc{record = ogn_collector_aircraft_position:set_value(Record, device, Data)};

create_aircraft_position({message_format, Data}, #acc{record = Record} = Acc) ->
    Acc#acc{record = ogn_collector_aircraft_position:set_value(Record, message_format, Data)};

create_aircraft_position({receiver, Data}, #acc{record = Record} = Acc) ->
    Acc#acc{record = ogn_collector_aircraft_position:set_value(Record, receiver, Data)};

create_aircraft_position({timestamp, Data}, #acc{record = Record} = Acc) ->
    % todo parse time
    Acc#acc{record = ogn_collector_aircraft_position:set_value(Record, timestamp, Data)};

create_aircraft_position({latitude, Data}, #acc{record = Record, parsers = Parsers} = Acc) ->
    Lat = parser_tool:list_to_latlon(Data, maps:get(latlon, Parsers)),
    Acc#acc{record = ogn_collector_aircraft_position:set_value(Record, latitude, Lat)};

create_aircraft_position({longitude, Data}, #acc{record = Record, parsers = Parsers} = Acc) ->
    Lon = parser_tool:list_to_latlon(Data, maps:get(latlon, Parsers)),
    Acc#acc{record = ogn_collector_aircraft_position:set_value(Record, longitude, Lon)};

create_aircraft_position({heading, Data}, #acc{record = Record} = Acc) ->
    Heading = {list_to_integer(Data), d},
    Acc#acc{record = ogn_collector_aircraft_position:set_value(Record, heading, Heading)};

create_aircraft_position({ground_speed, Data}, #acc{record = Record} = Acc) ->
    GroundSpeed = {list_to_integer(Data), kmph},
    Acc#acc{record = ogn_collector_aircraft_position:set_value(Record, ground_speed, GroundSpeed)};

create_aircraft_position({altitude, Data}, #acc{record = Record} = Acc) ->
    Altitude = {list_to_integer(Data), foot},
    Acc#acc{record = ogn_collector_aircraft_position:set_value(Record, altitude, Altitude)};

create_aircraft_position({comment, Data}, #acc{record = Record, parsers = Parsers} = Acc) ->
    {DeviceId, ClimbRate, TurnRate} = 
        parser_tool:parse_aircraft_comment(Data, maps:get(aircraft_comment, Parsers)),
    Acc#acc{record = ogn_collector_aircraft_position:set_values(Record, [
        {device_id, DeviceId},
        {climb_rate, ClimbRate},
        {turn_rate, TurnRate}
    ])};

create_aircraft_position(_, Acc) ->
    Acc.

