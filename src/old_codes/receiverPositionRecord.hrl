%% @author Janos Gergely <gergelyjanos@td1990.org>
%% @doc Receiver position record
%% @copyright 2019 TD*1990
%% @version 1.0.0

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
