-module(ecommerl_template).

%% API functions
-export([dir/1, layout_file/1, render/1, render/2, view_socket/1, elem_id/1]).

%% Includes
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

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

elem_id(Html) ->
    elem_id(Html, undefined, <<>>).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

elem_id(<<$<, T/binary>>, undefined, <<>>) ->
    elem_id(T, elem, <<>>);
elem_id(<<$>, _/binary>>, elem, <<>>) ->
    undefined;
elem_id(<<32, T/binary>>, undefined, <<>>) ->
    elem_id(T, undefined, <<>>);
elem_id(<<$i, $d, $=, $", T/binary>>, elem, <<>>) ->
    elem_id(T, id, <<>>);
elem_id(<<$i, $d, $=, $', T/binary>>, elem, <<>>) ->
    elem_id(T, id, <<>>);
elem_id(<<_, T/binary>>, elem, <<>>) ->
    elem_id(T, elem, <<>>);
elem_id(<<$", _/binary>>, id, Acc) ->
    Acc;
elem_id(<<$', _/binary>>, id, Acc) ->
    Acc;
elem_id(<<H, T/binary>>, id, Acc) ->
    elem_id(T, id, <<Acc/binary, H>>);
elem_id(_, undefined, <<>>) ->
    undefined.

%%%=============================================================================
%%% Tests
%%%=============================================================================

-ifdef(TEST).

elem_id_test() ->
    [?assertEqual(<<"app">>, elem_id(<<"<main id='app'>">>)),
     ?assertEqual(<<"app">>, elem_id(<<"<main id=\"app\">">>)),
     ?assertEqual(<<"app">>, elem_id(<<"<main  class='main'  id='app' foo>">>)),
     ?assertEqual(undefined, elem_id(<<>>)),
     ?assertEqual(undefined, elem_id(<<"<ul><li id='foo'>">>))].

-endif.
