
-module(echo_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Listen) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Listen]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

%%
%%init([Listen]) ->
%%    {ok, {{one_for_one, 5, 10},%% [{echo_sup_resender_one, {echo_sup_resender, start_link, [Listen]} ,permanent, 5000, worker, [echo_sup_resender]}
%%                                 {echo_acceptor_second, {echo_acceptor, start_link, []} ,permanent, 5000, worker, [echo_acceptor]}
%%                                }}.


init([Listen]) ->
    {ok, {{one_for_one, 5, 10}, 
            [{echo_sup_for_resender_one, {echo_sup_for_resender, start_link, []}, permanent, 5000, supervisor, [echo_sup_for_resender]},
             {echo_acceptor_one, {echo_acceptor, start_link, [Listen]} ,permanent, 5000, worker, [echo_acceptor]}
%%           {echo_acceptor_2, {echo_acceptor, start_link, [Listen]} ,permanent, 5000, worker, [echo_acceptor]}
                                                                     ]}}.
