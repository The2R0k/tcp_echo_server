-module(echo).

-export([start/0]).


start() ->
    application:start(echo).
