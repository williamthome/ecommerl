-module(ecommerl_template).

%% API functions
-export([dir/1, layout_file/1, render/1, render/2, view_socket/1]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

dir(Filename) ->
    ecommerl_app:priv_dir(["templates", Filename]).

layout_file(Filename) ->
    ecommerl_app:priv_dir(["templates", "layout", Filename]).

render(Socket) ->
    render(Socket, #{}).

render(Socket, Opts) ->
    {view, View, Params} = ecommerl_socket:route(Socket),
    Bindings = ecommerl_socket:bindings(Socket),
    Html0 = apply(View, render, [Bindings]),
    case maps:get(skip_layout, Opts, false) of
        true ->
            Html0;
        false ->
            case maps:find(layout, Params) of
                {ok, LayoutMod} ->
                    apply(LayoutMod, render, [
                        maps:merge(Bindings, #{inner_content => Html0})]);
                error ->
                    Html0
            end
    end.

view_socket({view, View, Params} = Route) ->
    Socket0 = ecommerl_socket:new(Route),
    {ok, Socket1} = apply(View, mount, [Params, Socket0]),
    {noreply, Socket} = apply(View, handle_params, [Params, Socket1]),
    Socket.
