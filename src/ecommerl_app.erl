%%%-------------------------------------------------------------------
%% @doc ecommerl public API
%% @end
%%%-------------------------------------------------------------------

-module(ecommerl_app).

-behaviour(application).

-export([start/2, stop/1]).
-export([priv_dir/0, priv_dir/1]).

start(_StartType, _StartArgs) ->
    ok = ecommerl_server:start(),
    ecommerl_sup:start_link().

stop(_State) ->
    ok.

priv_dir() ->
    code:priv_dir(ecommerl).

priv_dir(Path) when is_list(Path) ->
    filename:join([priv_dir() | Path]).

%% internal functions
