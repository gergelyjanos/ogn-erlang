-module(ogn_repo_map_worker).
-behaviour(gen_server).

-export([start/0, start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3]).

-include("../ogn_collector.hrl").

-define(SERVER, ?MODULE).

start() ->
   gen_server:start({local, ?SERVER}, ?MODULE, [], []).

start_link() ->
   gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init(_Args) ->
   {ok, #{aircrafts => #{}, receivers => #{}}}.

handle_call(db_size, _From, #{aircrafts := Aircrafts, receivers := Receivers} = State) ->
   Size = #{
      aircraft_map_size => maps:size(Aircrafts),
      receiver_map_size => maps:size(Receivers)
   },
   {reply, Size, State};
handle_call(stop, _From, State) ->
   {stop, normal, stopped, State}.

handle_cast({update_aircraft, #{aircraft_id := AircraftId} = Record}, #{aircrafts := Aircrafts} = State) ->
   Aircraft0 = maps:get(AircraftId, Aircrafts, #{}),
   Aircraft1 = maps:merge(Aircraft0, Record),
   {noreply, State#{aircrafts := Aircrafts#{AircraftId => Aircraft1}}};

handle_cast({update_receiver, #{receiver_id := ReceiverId} = Record}, #{receivers := Receivers} = State) ->
   Receiver0 = maps:get(ReceiverId, Receivers, #{}),
   Receiver1 = maps:merge(Receiver0, Record),
   {noreply, State#{receivers := Receivers#{ReceiverId => Receiver1}}}.

terminate(_Reason, _State) ->
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.
