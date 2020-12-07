-module(ogn_parser_receiver_status_parser).

-export([create_receiver_status_record/2]).

-include("./ogn_parser_pattern.hrl").

-define(RUN_OPTIONS, [{capture, all_but_first, list}]).

create_receiver_status_record(LineValues, Parsers) ->
    create_record(LineValues, Parsers, #{}).

%% @private
create_record([], _, Record) -> 
    Record;
create_record([{?TIMESTAMP_KEY, Value} | Tail], Parsers, Record) ->
    %% todo timestamp
    create_record(Tail, Parsers, Record#{?TIMESTAMP_KEY => Value});
create_record([{?RAM_KEY, Value} | Tail], #{ram := RamParser} = Parsers, Record) ->
    Ram = parse_ram(Value, RamParser),
    create_record(Tail, Parsers, Record#{?RAM_KEY => {Ram, Value}});
create_record([{Key, Value} | Tail], Parsers, Record) ->
    create_record(Tail, Parsers, Record#{Key => Value}).

%% @private
parse_ram(Data, {Regex, _}) ->
    case re:run(Data, Regex, ?RUN_OPTIONS) of
        {match, [UsedRam, AllRam, RamUnit]} ->
            {RamUnit, list_to_float(UsedRam), list_to_float(AllRam)};
        nomatch -> 
            {"", 0.0, 0.0}
    end.
