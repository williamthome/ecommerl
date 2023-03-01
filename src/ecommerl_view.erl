-module(ecommerl_view).

-callback mount(Params :: map(), Socket :: map()) -> {ok, Socket :: map()}.

-callback handle_params(Params :: map(), Socket :: map()) -> {noreply, Socket :: map()}.

-callback render(Bindings :: map()) -> binary().

-optional_callbacks([mount/2, handle_params/2]).
