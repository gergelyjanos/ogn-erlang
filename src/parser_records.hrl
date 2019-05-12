%% @author Janos Gergely <gergelyjanos@td1990.org>
%% @doc Records
%% @copyright 2019 TD*1990
%% @version 1.0.0

-record(aircraftPosition, 
    {
        device, 
        messageFormat, 
        receiver, 
        time, 
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
        time, 
        latitude, 
        longitude, 
        altitude    
    }).

-record(receiverStatus, 
    {
        receiver, 
        messageFormat, 
        server, 
        time, 
        version, 
        cpu, 
        ram, 
        other    
    }).
