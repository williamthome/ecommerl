-module(ecommerl_template).

%% API functions
-export([dir/1, layout_dir/1]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

dir(Filename) ->
    ecommerl_app:priv_dir(["templates", Filename]).

layout_dir(Filename) ->
    ecommerl_app:priv_dir(["templates", "layout", Filename]).
