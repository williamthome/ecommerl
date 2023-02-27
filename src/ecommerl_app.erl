%%%-------------------------------------------------------------------
%% @doc ecommerl public API
%% @end
%%%-------------------------------------------------------------------

-module(ecommerl_app).

-behaviour(application).

-export([start/2, stop/1]).
-export([priv_dir/0]).

start(_StartType, _StartArgs) ->
    ok = ecommerl_server:start(),
    ecommerl_sup:start_link().

stop(_State) ->
    ok.

priv_dir() ->
    code:priv_dir(ecommerl).

%% internal functions
