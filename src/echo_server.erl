-module(echo_server).

-behaviour(gen_server).

-include("echo.hrl").
-include_lib("chronica/include/chronica.hrl").

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {socket}).

start_link(Client) ->
    gen_server:start_link(?MODULE, [Client], []).


init([Client]) ->
    {ok, #state{socket=Client}}.


handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info({tcp, Socket, Data}, State=#state{socket=Socket}) ->
	gen_tcp:send(Socket, Data),
	inet:setopts(Socket, [{active, once}]),
	{noreply, State, ?CLIENT_TIMEOUT};
handle_info({tcp_closed, _Socket}, State) ->
    log:info("Client close chanel"),
	{stop, normal, State};
handle_info({tcp_error, _, Reason}, State) ->
    log:error("ERROR ~p", [Reason]),
	{stop, Reason, State};
handle_info(timeout, State) ->
	log:info("Client don't send msg too late / Timeout"),
    {stop, normal, State}.


terminate(_, _) ->
    ok.


code_change(_, State, _) ->
    {ok, State}.
