-module(pattern).

-export([get_pattern/1]).

-define(AIRCRAFT_POSITION_PATTERN, "^(?P<device>.+)>(?P<message_format>.+),.+,(?P<receiver>.+):\\/(?P<timestamp>\\d+)h(?P<latitude>\\d+\\.\\d+[NS])[\\/\\\\]+(?P<longitude>\\d+\\.\\d+[EW])\\S(?P<heading>\\d+)\\/(?P<ground_speed>\\d+)\\/A=(?P<altitude>\\d+)\\s+(?P<comment>.*)\\s*$").
-define(AIRCRAFTCOMMENT_PATTERN, "^!\\S+!\\s+(?P<device_id>id[0-9a-fA-F]{8})\\s+(?P<climb_rate>[+-]*\\d+).p.\\s+(?P<turn_rate>[+-]*[\\d\\.]+)rot\\s*.*$").
-define(RECEIVERPOSITION_PATTERN, "^(?P<receiver>.+)>(?P<message_format>.+),.+,.+,(?P<server>.+):\\/(?P<timestamp>\\d+)h(?P<latitude>\\d+\\.\\d+[NS])[\\/\\\\I]+(?P<longitude>\\d+\\.\\d+[EW])[\\/\\&]+A=(?P<altitude>\\d+)$").
-define(RECEIVERSTATUS_PATTERN, "^(?P<receiver>.+)>(?P<message_format>.+),.+,.+,(?P<server>.+):>(?P<timestamp>\\d+)h\\s+(?P<version>.+)\\s+CPU:(?P<cpu>\\d+\\.\\d+)\s+RAM:(?P<ram>\\S+)\\s+(?P<other>.*)$").
-define(TIMESTAMP_PATTERN, "^(\\d\\d)(\\d\\d)(\\d\\d)$").

get_pattern(aircraft_position) -> 
    ?AIRCRAFT_POSITION_PATTERN;
get_pattern(aircraft_comment) ->
    ?AIRCRAFTCOMMENT_PATTERN;
get_pattern(receiver_position) ->
    ?RECEIVERPOSITION_PATTERN;
get_pattern(receiver_status) ->
    ?RECEIVERSTATUS_PATTERN;
get_pattern(timestamp) ->
    ?TIMESTAMP_PATTERN.

