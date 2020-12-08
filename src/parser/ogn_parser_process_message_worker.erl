-module(ogn_parser_process_message_worker).
-behaviour(gen_server).

%% API
-export([start/1, start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([parse_raw_line/2]).

-include("../ogn_collector.hrl").
-include("ogn_parser_pattern.hrl").


%% api

parse_raw_line(<<"#", Comment/binary>>, Parsers) ->
   {ok, Pid} = ogn_parser_process_message_sup:start_parser_worker(Parsers),
   gen_server:cast(Pid, {comment, Comment}),
   ok;
parse_raw_line(Line, Parsers) ->
   {ok, Pid} = ogn_parser_process_message_sup:start_parser_worker(Parsers),
   gen_server:cast(Pid, {line, Line}),
   ok.

%% gen server api

start(Parsers) ->
   gen_server:start(?MODULE, [Parsers], []).

start_link() ->
   gen_server:start_link(?MODULE, [], []).

init([Parsers]) ->
   {ok, #{parsers => Parsers}}.

handle_call(stop, _From, State) ->
   {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({server_name, _ServerName}, State) ->
   {stop, normal, State};
handle_cast({line, Line}, #{parsers := Parsers} = State) ->
   process_parse_line_result(ogn_parser_line_parser:parse_line(Line, Parsers)),
   {stop, normal, State};
handle_cast({comment, Comment}, State) ->
   ?LOG_DEBUG("~p:cast comment ~p", [?MODULE, Comment]),
   {stop, normal, State}.

handle_info(_Info, State) ->
   {stop, normal, State}.

terminate(_Reason, _State) ->
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.

process_parse_line_result(nomatch) -> 
   ok;
process_parse_line_result({aircraft_position, #{?DEVICE_KEY := Device} = Position}) ->
   Record0 = 
   #{
      aircraft_id => Device,
      geo_coord => get_geo_coord(?GEO_COORD_KEYS, Position, #{}),
      speed => get_speed(?SPEED_KEYS, Position, #{})
   },
   Record1 = add_aircraft_data(?AIRCRAFT_DATA_KEYS, Position, Record0),
   ogn_repo:update_aircraft(Record1),
   ok;
process_parse_line_result({receiver_position, Record}) ->
   ?LOG_DEBUG("--- RECEIVER POS ~p", [Record]),
   ok;
process_parse_line_result({receiver_status, Record}) ->
   ?LOG_DEBUG("+++ RECEIVER STA ~p", [Record]),
   ok. 

get_geo_coord([?LATITUDE_KEY | T], #{?LATITUDE_KEY := {Latitude, Lat}} = Position, GeoCoord) ->
   get_geo_coord(T, Position, GeoCoord#{latitude => Latitude, lat => Lat});
get_geo_coord([?LONGITUDE_KEY | T], #{?LONGITUDE_KEY := {Longitude, Lon}} = Position, GeoCoord) ->
   get_geo_coord(T, Position, GeoCoord#{longitude => Longitude, lon => Lon});
get_geo_coord([?ALTITUDE_KEY | T], #{?ALTITUDE_KEY := Altitude} = Position, GeoCoord) ->
   get_geo_coord(T, Position, GeoCoord#{altitude => Altitude});
get_geo_coord([_ | T], Position, GeoCoord) ->
   get_geo_coord(T, Position, GeoCoord);
get_geo_coord([], _, GeoCoord) ->
   GeoCoord.

get_speed([?GROUND_SPEED_KEY | T], #{?GROUND_SPEED_KEY := GroundSpeed} = Position, Speed) ->
   get_speed(T, Position, Speed#{ground_speed => GroundSpeed});
get_speed([?HEADING_KEY | T], #{?HEADING_KEY := Heading} = Position, Speed) ->
   get_speed(T, Position, Speed#{heading => Heading});
get_speed([?CLIMB_RATE_KEY | T], #{?CLIMB_RATE_KEY := ClimbRate} = Position, Speed) ->
   get_speed(T, Position, Speed#{climb_rate => ClimbRate});
get_speed([?TURN_RATE_KEY | T], #{?TURN_RATE_KEY := TurnRate} = Position, Speed) ->
   get_speed(T, Position, Speed#{turn_rate => TurnRate});
get_speed([_ | T], Position, Speed) ->
   get_speed(T, Position, Speed);
get_speed([], _, Speed) ->
   Speed.

add_aircraft_data([?MESSAGE_FORMAT_KEY | T], #{?MESSAGE_FORMAT_KEY := Value} = Position, Record) ->
   add_aircraft_data(T, Position, Record#{message_format => Value});
add_aircraft_data([?RECEIVER_KEY | T], #{?RECEIVER_KEY := Value} = Position, Record) ->
   add_aircraft_data(T, Position, Record#{receiver => Value});
add_aircraft_data([?DEVICE_ID_KEY | T], #{?DEVICE_ID_KEY := Value} = Position, Record) ->
   add_aircraft_data(T, Position, Record#{device_id => Value});
add_aircraft_data([?TIMESTAMP_KEY | T], #{?TIMESTAMP_KEY := Value} = Position, Record) ->
   add_aircraft_data(T, Position, Record#{time_stamp => Value});
add_aircraft_data([_ | T], Position, Record) ->
   add_aircraft_data(T, Position, Record);
add_aircraft_data([], _, Record) ->
   Record.
