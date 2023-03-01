-module(ecommerl_view_error404).

%% API functions
-export([render/1]).

%% Includes
-include("ecommerl_view.hrl").

%%%=============================================================================
%%% API functions
%%%=============================================================================

render(Bindings) ->
    ?H("Sorry, content not found! =(").
