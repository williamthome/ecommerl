-module(ecommerl_router).

%% API functions
-export([route/2]).

%% Defines
%% TODO: Get layout from pipeline
-define(LAYOUT, ecommerl_view_app).
-define(DEFAULTS_OPTS, #{layout => ?LAYOUT}).
-define(DEFAULTS_OPTS(Opts), maps:merge(?DEFAULTS_OPTS, Opts)).

%%%=============================================================================
%%% API functions
%%%=============================================================================

route(<<"GET">>, <<"/">>) ->
    {view, ecommerl_view_home, ?DEFAULTS_OPTS};
route(<<"GET">>, _) ->
    {view, ecommerl_view_error404, ?DEFAULTS_OPTS(#{status_code => 400})}.
