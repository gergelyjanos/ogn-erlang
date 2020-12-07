%% @author Janos Gergely <gergelyjanos@td1990.org>
%% @doc Aircraft position record
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

