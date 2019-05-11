%% @author Janos Gergely <gergelyjanos@td1990.org>
%% @doc Records
%% @copyright 2019 TD*1990
%% @version 1.0.0

-record(aircraft, 
    {
        device, 
        messageFormat, 
        receiver, 
        timestamp, 
        latitude, 
        longitude, 
        heading, 
        groundSpeed, 
        altitude, 
        deviceId, 
        climbRate, 
        turnRate
    }).

-record(receiverPosition, 
    {
        receiver, 
        messageFormat, 
        server, 
        timestamp, 
        latitude, 
        longitude, 
        altitude    
    }).

-record(receiverStatus, 
    {
        receiver, 
        messageFormat, 
        server, 
        timestamp, 
        version, 
        cpu, 
        ram, 
        other    
    }).
