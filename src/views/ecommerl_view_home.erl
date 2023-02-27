-module(ecommerl_view_home).

%% API functions
-export([mount/2, render/1]).

%% Includes
-include("ecommerl_view.hrl").

%%%=============================================================================
%%% API functions
%%%=============================================================================

mount(_Params, Socket0) ->
    Socket = bind(name, <<"World">>, Socket0),
    {ok, Socket}.

render(Bindings) ->
    ?H("Hello, <%= Name .%>!").
