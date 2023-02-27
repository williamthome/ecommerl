-module(ecommerl_view).

-callback mount(Params :: map(), Socket :: map()) -> {ok, Socket :: map()}.

-callback render(Bindings :: map()) -> binary().
