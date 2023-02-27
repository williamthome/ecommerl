-module(ecommerl_server).

%% API functions
-export([start/0]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

start() ->
    Routes = [
        {'_', [
            {"/css/[...]", cowboy_static, {priv_dir, ecommerl, "static/css"}},
            {"/js/[...]", cowboy_static, {priv_dir, ecommerl, "static/js"}},
            {"/favicon.ico", cowboy_static, {priv_file, ecommerl, "static/favicon.ico"}},
            {'_', ecommerl_handler, #{}}
        ]}
    ],
    Dispatch = cowboy_router:compile(Routes),
    persistent_term:put(ecommerl_dispatch, Dispatch),
    {ok, _} = cowboy:start_clear(
        ecommerl_http_listener,
        [{port, 8080}],
        #{env => #{dispatch => {persistent_term, ecommerl_dispatch}}}
    ),
    ok.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

% nothing here yet!
