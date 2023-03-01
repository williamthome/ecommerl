-module(ecommerl_server).

%% API functions
-export([start/1]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

start(Routes) ->
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
