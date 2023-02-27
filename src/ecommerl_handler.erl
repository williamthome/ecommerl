-module(ecommerl_handler).

-behaviour(cowboy_handler).

%% cowboy_handler callbacks
-export([init/2, terminate/3]).

%%%=============================================================================
%%% cowboy_handler callbacks
%%%=============================================================================

-spec init(Req, State) ->
    {ok | module(), Req, State} | {module(), Req, term(), term()} when
    Req :: cowboy_req:req(),
    State :: term().

init(Req, State) ->
    Method = cowboy_req:method(Req),
    Path = cowboy_req:path(Req),
    handle(Method, Path, Req, State).

-spec terminate(term(), map(), term()) -> ok.

terminate(_Reason, _Req, _State) ->
    ok.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

handle(Method, Path, Req0, State) ->
    Route = ecommerl_router:route(Method, Path),
    Req = reply(Route, Req0),
    {ok, Req, State}.

reply({view, Mod, Params}, Req) ->
    Socket0 = ecommerl_socket:new(Mod, Params),
    {ok, Socket} = apply(Mod, mount, [Params, Socket0]),
    #{bindings := Bindings} = Socket,
    Html0 = apply(Mod, render, [Bindings]),
    Html =
        case maps:find(layout, Params) of
            {ok, LayoutMod} ->
                apply(LayoutMod, render, [
                    maps:merge(Bindings, #{inner_content => Html0})]);
            error ->
                Html0
        end,
    Headers = maps:merge(maps:get(headers, Params, #{}), #{
        <<"content-type">> => <<"text/html">>
    }),
    cowboy_req:reply(
        maps:get(status_code, Params, 200),
        Headers,
        Html,
        Req
    ).
