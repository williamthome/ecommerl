%%%-------------------------------------------------------------------
%% @doc ecommerl public API
%% @end
%%%-------------------------------------------------------------------

-module(ecommerl_app).

-behaviour(application).

-export([start/2, stop/1]).
-export([priv_dir/0, priv_dir/1]).

start(_StartType, _StartArgs) ->
    Routes = [
        {'_', [
            {"/css/[...]", cowboy_static, {priv_dir, ecommerl, "static/css"}},
            {"/js/[...]", cowboy_static, {priv_dir, ecommerl, "static/js"}},
            {"/favicon.ico", cowboy_static, {priv_file, ecommerl, "static/favicon.ico"}},
            {"/websocket", ecommerl_ws, #{}},
            {'_', ecommerl_handler, #{}}
        ]}
    ],
    ok = ecommerl_server:start(Routes),
    ecommerl_sup:start_link().

stop(_State) ->
    ok.

priv_dir() ->
    code:priv_dir(ecommerl).

priv_dir(Path) when is_list(Path) ->
    filename:join([priv_dir() | Path]).

%% internal functions
