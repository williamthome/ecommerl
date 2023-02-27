-module(ecommerl_view).

-callback mount(Params :: map(), Socket :: map()) -> {ok, Socket :: map()}.

-callback render(Bindings :: map()) -> binary().

-optional_callbacks([mount/2]).
