-module(ecommerl_view_app).

%% API functions
-export([mount/2, render/1]).

%% Includes
-include("ecommerl_view.hrl").

%%%=============================================================================
%%% API functions
%%%=============================================================================

mount(_Params, Socket) ->
    {ok, Socket}.

%% TODO: Relative path
render(Bindings) ->
    ?F("/home/williamthome/Projects/erlang/ecommerl/priv/templates/layout/app.html.eel").
