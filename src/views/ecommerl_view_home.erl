-module(ecommerl_view_home).

%% API functions
-export([render/1, handle_event/3]).

%% Includes
-include("ecommerl_view.hrl").

%%%=============================================================================
%%% API functions
%%%=============================================================================

render(Bindings) -> ?H("
<main id='app'>
    <%= case maps:find('Name', Bindings) of %>
    <% {ok, Name} -> %>
        <span>Hello, <%= Name .%>!</span>
        <button
            type='button'
            onclick='ping()'
        >
            Ping
        </button>
    <% ; error -> %>
        <span>Nobody's home =(</span>
        <button
            type='button'
            data-event='set_name'
            data-name='World'
        >
            Fire!
        </button>
    <% end .%>
</main>
").

handle_event(<<"ping">>, #{}, Socket) ->
    {reply, <<"pong">>, Socket};
handle_event(<<"set_name">>, #{name := Name}, Socket0) ->
    Socket = ecommerl_socket:bind(name, Name, Socket0),
    {noreply, Socket}.
