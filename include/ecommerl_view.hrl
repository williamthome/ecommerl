-behaviour(ecommerl_view).

-compile({parse_transform, ecommerl_trans}).

-import(ecommerl_socket, [bind/3, bind_new/3]).

-define(H(Html), {Html, Bindings}).
