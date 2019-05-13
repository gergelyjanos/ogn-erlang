%% @author Janos Gergely <gergelyjanos@td1990.org>
%% @doc Records
%% @copyright 2019 TD*1990
%% @version 1.0.0

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
