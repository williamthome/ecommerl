-module(ecommerl_view_home).

%% API functions
-export([mount/2, render/1]).

%% Includes
-include("ecommerl_view.hrl").

%%%=============================================================================
%%% API functions
%%%=============================================================================

render(Bindings) -> ?H("
<main id=\"app\">
    <%= case maps:find('Name', Bindings) of %>
    <%     {ok, Name} -> %>
        <span>Hello, <%= Name .%>!</span>
        <button
            type='button'
            onclick='app.socket.send(`ping`)'
        >
            Ping
        </button>
    <%  ;  error -> %>
        <span>Nobody's home =(</span>
        <button
            type='button'
            onclick='app.socket.send(`render`, {name: `World`})'
        >
            Fire!
        </button>
    <% end .%>
</main>
").
