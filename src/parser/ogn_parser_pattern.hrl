
-define(AIRCRAFT_POSITION_PATTERN, "^(?P<device>.+)>(?P<message_format>.+),.+,(?P<receiver>.+):\\/(?P<timestamp>\\d+)h(?P<latitude>\\d+\\.\\d+[NS])[\\/\\\\]+(?P<longitude>\\d+\\.\\d+[EW])\\S(?P<heading>\\d+)\\/(?P<ground_speed>\\d+)\\/A=(?P<altitude>\\d+)\\s+(?P<comment>.*)\\s*$").
-define(AIRCRAFTCOMMENT_PATTERN, "^!\\S+!\\s+(?P<device_id>id[0-9a-fA-F]{8})\\s+(?P<climb_rate>[+-]*\\d+).p.\\s+(?P<turn_rate>[+-]*[\\d\\.]+)rot\\s*.*$").

%% <<"bSkyN1002>OGNFNT,TCPIP*,qAC,GLIDERN5:/121054h4710.86NI01152.23E&/A=001575\r\n">>
%% <<"UKFES>APRS,TCPIP*,qAC,GLIDERN1:/121054h5706.37NI00353.37W&/A=000889\r\n">>
-define(RECEIVERPOSITION_PATTERN, "^(?P<receiver>.+)>(?P<message_format>.+),.+,.+,(?P<server>.+):\\/(?P<timestamp>\\d+)h(?P<latitude>\\d+\\.\\d+[NS])[\\/\\\\I]+(?P<longitude>\\d+\\.\\d+[EW])\\&.*\\/+A=(?P<altitude>\\d+)\\s*(?P<other>.*)$").

%% <<"EPGI1>APRS,TCPIP*,qAC,GLIDERN1:/134230h5331.13NI01851.00E&000/000/A=000108 v0.2.3.x86 CPU:0.0 RAM:226.7/4082.5MB NTP:0.9ms/+64.8ppm +29.8C\r\n">>
%% <<"LesOrres>APRS,TCPIP*,qAC,GLIDERN1:/131234h4429.61NI00633.39E&/A=005376 v0.2.5.ARM CPU:0.3 RAM:632.1/970.5MB NTP:1.0ms/-2.4ppm +45.1C RF:+45-1.2ppm/-1.32dB\r\n">>
%% <<"EPGI1>APRS,TCPIP*,qAC,GLIDERN1:/134730h5331.13NI01851.00E&000/000/A=000108 v0.2.3.x86 CPU:0.0 RAM:227.2/4082.5MB NTP:0.9ms/+64.8ppm +29.8C\r\n">>
-define(RECEIVERPOSITIONSTATUS_PATTERN, "^(?P<receiver>.+)>(?P<message_format>.+),.+,.+,(?P<server>.+):\\/(?P<timestamp>\\d+)h(?P<latitude>\\d+\\.\\d+[NS])[\\/\\\\I]+(?P<longitude>\\d+\\.\\d+[EW])\\&.*\\/A=(?P<altitude>\\d+)\\s+(?P<version>.+)\\s+CPU:(?P<cpu>\\d+\\.\\d+)\s+RAM:(?P<ram>\\S+)\\s+(?P<other>.*)$").

-define(RECEIVERSTATUS_PATTERN, "^(?P<receiver>.+)>(?P<message_format>.+),.+,.+,(?P<server>.+):>(?P<timestamp>\\d+)h\\s+(?P<version>.+)\\s+CPU:(?P<cpu>\\d+\\.\\d+)\s+RAM:(?P<ram>\\S+)\\s+(?P<other>.*)$").

-define(TIMESTAMP_PATTERN, "^(\\d\\d)(\\d\\d)(\\d\\d)$").
-define(LATLON_PATTERN, "^(?P<degree>\\d{2,3})(?P<minute>\\d{2})\\.(?P<second>\\d{2})(?P<globe>[EWSN])$").
-define(RAM_PATTERN, "^(?P<used_ram>[0-9\\.]+)\\/(?P<all_ram>[0-9\\.]+)(?P<ram_unit>.+)$").


-define(DEVICE_KEY, <<"device">>).
-define(MESSAGE_FORMAT_KEY, <<"message_format">>).
-define(RECEIVER_KEY, <<"receiver">>).
-define(TIMESTAMP_KEY, <<"timestamp">>).
-define(LATITUDE_KEY, <<"latitude">>).
-define(LONGITUDE_KEY, <<"longitude">>).
-define(HEADING_KEY, <<"heading">>).
-define(GROUND_SPEED_KEY, <<"ground_speed">>).
-define(ALTITUDE_KEY, <<"altitude">>).
-define(COMMENT_KEY, <<"comment">>).
-define(DEVICE_ID_KEY, <<"device_id">>).
-define(CLIMB_RATE_KEY, <<"climb_rate">>).
-define(TURN_RATE_KEY, <<"turn_rate">>).
-define(SERVER_KEY, <<"server">>).
-define(VERSION_KEY, <<"version">>).
-define(CPU_KEY, <<"cpu">>).
-define(OTHER_KEY, <<"other">>).
-define(DEGREE_KEY, <<"degree">>).
-define(MINUTE_KEY, <<"minute">>).
-define(SECOND_KEY, <<"second">>).
-define(GLOBE_KEY, <<"globe">>).
-define(RAM_KEY, <<"ram">>).
-define(USED_RAM_KEY, <<"used_ram">>).
-define(ALL_RAM_KEY, <<"all_ram">>).
-define(RAM_UNIT_KEY, <<"ram_unit">>).

% -define(GEO_COORD_KEYS, [?LONGITUDE_KEY, ?LATITUDE_KEY, ?ALTITUDE_KEY]).
% -define(SPEED_KEYS, [?HEADING_KEY, ?GROUND_SPEED_KEY, ?CLIMB_RATE_KEY, ?TURN_RATE_KEY]).
