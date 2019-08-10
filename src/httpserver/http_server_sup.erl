%%%--------------------------------------------------------------------- 
%%% TD*1990
%%%--------------------------------------------------------------------- 

%% REMOVE IT

-module(http_server_sup).
-behavior(supervisor).
-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link(?MODULE, []).

init(_Args) ->
    RestartStrategy = #{strategy => one_for_one, intensity => 1, period => 5},
    Server = #{id => http_server_server,
                    start => {http_server_server, start_link, []},
                    restart => permanent,
                    shutdown => brutal_kill,
                    type => worker,
                    modules => [http_server_server]},
    Children = [Server],
    {ok, {RestartStrategy, Children}}.


    % HttpServerSupervisor=#{id => http_server_sup,
    %         start => {http_server_sup, start_link, []},
    %         restart => permanent,
    %         shutdown => brutal_kill,
    %         type => supervisor,
    %         modules => [http_server_sup]},
