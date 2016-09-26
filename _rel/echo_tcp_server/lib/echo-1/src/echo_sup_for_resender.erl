-module(echo_sup_for_resender).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Name, Type, Args), {Name, {Name, start_link, Args}, transient, 5000, Type, [Name]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

%%init(_Args) ->
%%
%% {ok, {{simple_one_for_one, 5, 10}, []}}.%%{echo_resender_one, {echo_server, start_link, []} ,permanent, 5000, worker, [echo_server]}]}}.
%%
%%    {ok, { { one_for_one, 5, 10},{ echo_resender,{echo_server, start_link, []} ,transient, 5000, worker, [echo_server]} }}. 
%%    
%%    
%%     {echo_resender_one, {echo_resender, start_link, []}, transient, 10, worker,[echo_resender]}}}.


init(_Args) ->

    SupFlags = #{strategy => simple_one_for_one,
                    intensity => 0,
                    period => 1},
    ChildSpecs = [#{id => echo_resender,
                    start => {echo_server, start_link, []},
                    shutdown => brutal_kill}],
    {ok, {SupFlags, ChildSpecs}}.
