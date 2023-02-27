-module(ecommerl_view_error404).

%% API functions
-export([mount/2, render/1]).

%% Includes
-include("ecommerl_view.hrl").

%%%=============================================================================
%%% API functions
%%%=============================================================================

mount(_Params, Socket) ->
    {ok, Socket}.

render(Bindings) ->
    ?H("Sorry, content not found! =(").
