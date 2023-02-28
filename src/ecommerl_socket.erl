-module(ecommerl_socket).

%% API functions
-export([new/2, bindings/1, bind/3, bind_new/3]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

new(View, Opts) ->
    #{
        view => View,
        bindings => maps:get(bindings, Opts, #{})
    }.

bindings(#{bindings := Bindings}) ->
    Bindings.

bind(Key, Value, #{bindings := Bindings} = Socket) ->
    Socket#{bindings => Bindings#{Key => Value}}.

bind_new(Key, Fun, #{bindings := Bindings} = Socket) ->
    case maps:find(Key, Bindings) of
        {ok, _} -> Socket;
        error -> bind(Key, Fun(), Socket)
    end.
