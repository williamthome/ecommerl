%%%-------------------------------------------------------------------
%% @doc ecommerl public API
%% @end
%%%-------------------------------------------------------------------

-module(ecommerl_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ok = ecommerl_server:start(),
    ecommerl_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
