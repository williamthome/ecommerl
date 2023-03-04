-module(ecommerl_view_error404).

%% ecommerl_view callbacks
-export([render/1]).

%% Includes
-include("ecommerl_view.hrl").

%%%=============================================================================
%%% ecommerl_view callbacks
%%%=============================================================================

render(Bindings) ->
    ?H("Sorry, content not found! =(").
