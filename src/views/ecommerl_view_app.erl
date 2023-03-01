-module(ecommerl_view_app).

%% API functions
-export([render/1]).

%% Includes
-include("ecommerl_view.hrl").

%%%=============================================================================
%%% API functions
%%%=============================================================================

render(Bindings) ->
    ?F(ecommerl_template:layout_file("app.html.eel")).
