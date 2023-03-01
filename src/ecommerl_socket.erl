-module(ecommerl_socket).

%% API functions
-export([new/1, route/1, bindings/1, set_bindings/2, bind/3, bind_new/3]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

new(Route) ->
    #{
        route => Route,
        bindings => #{}
    }.

route(#{route := Route}) ->
    Route.

bindings(#{bindings := Bindings}) ->
    Bindings.

set_bindings(Bindings, Socket) ->
    Socket#{bindings => Bindings}.

bind(Key, Value, #{bindings := Bindings} = Socket) ->
    Socket#{bindings => Bindings#{Key => Value}}.

bind_new(Key, Fun, #{bindings := Bindings} = Socket) ->
    case maps:find(Key, Bindings) of
        {ok, _} -> Socket;
        error -> bind(Key, Fun(), Socket)
    end.
