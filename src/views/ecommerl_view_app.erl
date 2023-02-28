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
    ?F(ecommerl_template:layout_dir("app.html.eel")).
