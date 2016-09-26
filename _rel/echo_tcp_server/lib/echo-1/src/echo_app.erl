-module(echo_app).

-behaviour(application).

-include("echo.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================


start(_, _) ->
   {ok, Listen} = gen_tcp:listen(?PORT, [binary, {packet,0}, {active,true}, {reuseaddr, true}]),
   {ok, Pid} = echo_sup:start_link(Listen),
   {ok, Pid}.
    

stop(_) ->
    ok.
