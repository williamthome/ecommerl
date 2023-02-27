-module(ecommerl_router).

%% API functions
-export([route/2]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

route(<<"GET">>, <<"/">>) ->
    {view, ecommerl_view_home, #{}};
route(_, _) ->
    {view, ecommerl_view_error404, #{status_code => 400}}.
