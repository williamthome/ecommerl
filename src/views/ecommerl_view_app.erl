-module(ecommerl_view_app).

%% ecommerl_view callbacks
-export([render/1]).

%% Includes
-include("ecommerl_view.hrl").

%%%=============================================================================
%%% ecommerl_view callbacks
%%%=============================================================================

render(Bindings) ->
    ?F(ecommerl_template:layout_file("app.html.eel")).
