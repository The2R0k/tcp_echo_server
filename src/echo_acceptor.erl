-module(echo_acceptor).

-export([start_link/1, loop/1]).

-include_lib("chronica/include/chronica.hrl").

-define(CHILD(Type,Args), {[], {echo_resender, start_link, Args}, permanent, 5000, Type, [echo_resender]}).

start_link(Listen) ->
    Pid = spawn_link(fun() -> loop(Listen) end),
    {ok, Pid}.

loop(Listen) ->

        

            case gen_tcp:accept(Listen) of
                {ok, Client} ->
                    log:info("We are have a new user ~p", [Client]), 
                    {ok, Pid} = supervisor:start_child(echo_sup_for_resender, [Client]),
                    gen_tcp:controlling_process(Client,Pid),
                    loop(Listen);
                {error, Reason} ->
                      log:error("ERROR ~p", [Reason]),
                      {error, Reason}
            end.


%%%
%%%            gen_tcp:controlling_process(Client, Pid),
%%%            loop(Listen).
%%%
