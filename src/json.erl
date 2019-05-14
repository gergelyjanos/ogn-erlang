
-module(json).
-export([listOfRecordsToJson/2, integerToJson/2]).

listOfRecordsToJson(Fields, Records) ->
    % Fields = record_info(fields, aircraftPosition),
    
    List = lists:foldl(fun(Item, Results) -> 
            [RecordName | Values] = tuple_to_list(Item),
            lists:append(Results, [{RecordName, lists:zip(Fields, Values)}])
            % lists:append(Results, [
            %     io_lib:format("\{ ~s : \{ ~p\}\}", [RecordName, lists:zip(Fields, Values)])
            % ])
        end, 
        [],
        Records),
    io_lib:format("~p", [List])
.

integerToJson(Name, Value) ->
    io_lib:format("{'~s': ~p}\r\n", [Name, Value])
.