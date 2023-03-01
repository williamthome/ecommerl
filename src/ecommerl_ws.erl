-module(ecommerl_ws).

-behaviour(cowboy_websocket).

%% cowboy_websocket callbacks
-export([init/2,
         terminate/3,
         websocket_init/1,
         websocket_handle/2,
         websocket_info/2]).

%% Types
-type commands() :: cowboy_websocket:commands().
-type call_result(State) :: {commands(), State} | {commands(), State, hibernate}.

%% State
-record(state, {
    params,
    path,
    route,
    socket
}).

%%%=============================================================================
%%% cowboy_websocket callbacks
%%%=============================================================================

%% -----------------------------------------------------------------------------
%% @doc init/2.
%% @end
%% -----------------------------------------------------------------------------
-spec init(Req, any()) ->
    {ok | module(), Req, any()} | {module(), Req, any(), any()} when
    Req :: cowboy_req:req().

init(Req0, #{}) ->
    Params = cowboy_req:parse_qs(Req0),
    {<<"path">>, Path} = proplists:lookup(<<"path">>, Params),
    Route = ecommerl_router:route(<<"GET">>, Path),
    Socket = ecommerl_template:view_socket(Route),
    State = #state{
        params = cowboy_req:parse_qs(Req0),
        path = Path,
        socket = Socket
    },
    OneMinuteInMs = 60_000,
    WsConnTimeout = OneMinuteInMs * 10,
    Options = #{idle_timeout => WsConnTimeout},
    io:format("websocket connection initiated~n~p~n~nstate: ~p~n", [Req0, State]),
    case cowboy_req:parse_header(<<"sec-websocket-protocol">>, Req0) of
        undefined ->
            {cowboy_websocket, Req0, State, Options};
        Subprotocols ->
            case lists:keymember(<<"mqtt">>, 1, Subprotocols) of
                true ->
                    Req = cowboy_req:set_resp_header(
                        <<"sec-websocket-protocol">>,
                        <<"mqtt">>,
                        Req0
                    ),
                    {cowboy_websocket, Req, State, Options};
                false ->
                    Req = cowboy_req:reply(400, Req0),
                    {ok, Req, State}
            end
    end.

%% -----------------------------------------------------------------------------
%% @doc websocket_init/1.
%% @end
%% -----------------------------------------------------------------------------
-spec websocket_init(State) -> call_result(State) when State :: any().

websocket_init(State) ->
    io:format("init websocket [~p]: ~p~n", [self(), State]),
    reply(<<"ready">>, #{}, State).

%% -----------------------------------------------------------------------------
%% @doc websocket_handle/2.
%% @end
%% -----------------------------------------------------------------------------
-spec websocket_handle(ping
                      | pong
                      | {text | binary | ping | pong, binary()},
                      State) -> call_result(State).

websocket_handle({text, Msg}, State) ->
    handle(Msg, State).

%% -----------------------------------------------------------------------------
%% @doc websocket_info/2.
%% @end
%% -----------------------------------------------------------------------------
-spec websocket_info(any(), State) -> call_result(State) when State :: any().

websocket_info({notify, Event, Payload}, State) ->
    reply(Event, Payload, State).

-spec terminate(any(), cowboy_req:req(), any()) -> ok.

terminate(Reason, Req, _State) ->
    io:format(
        "websocket connection terminated~n~p~nReason: ~p~n",
        [maps:get(peer, Req), Reason]
    ),
    ok.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

handle(Msg, #state{socket = Socket0} = State) ->
    io:format("Got msg: ~p~n", [Msg]),
    {ok, Data} = ecommerl_utils:decode_json(Msg),
    case maps:get(<<"event">>, Data) of
        <<"ping">> ->
            reply(<<"pong">>, Data, State);
        _ ->
            Payload = normalize_payload(maps:get(<<"payload">>, Data)),
            SocketBindings = ecommerl_socket:bindings(Socket0),
            Bindings = maps:merge(SocketBindings, Payload),
            Socket = ecommerl_socket:set_bindings(Bindings, Socket0),
            Reply = ecommerl_template:render(Socket, #{skip_layout => true}),
            reply(<<"render">>, Reply, State)
    end.

normalize_payload(Payload) when is_map(Payload) ->
    maps:fold(fun(K, V, A) -> A#{binary_to_existing_atom(K) => V} end,
              #{},
              Payload);
normalize_payload(Payload) ->
    Payload.

reply(Event, Payload, State) ->
    Msg = ecommerl_utils:encode_json(#{
        <<"event">> => Event,
        <<"payload">> => Payload
    }),
    {reply, {text, Msg}, State}.
