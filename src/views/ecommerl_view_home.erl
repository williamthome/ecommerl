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
    Hello, <%= case maps:find('Name', Bindings) of
                   {ok, Name} -> Name;
                   error -> <<\"Nobody\">> end
           .%>!
    <button
        type='button'
        onclick='app.socket.send(`render`, {name: `World`})'
    >
        Fire!
    </button>
</main>
").
