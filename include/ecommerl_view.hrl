-behaviour(ecommerl_view).

-compile({parse_transform, ecommerl_trans}).

-import(ecommerl_socket, [bind/3, bind_new/3]).

-define(H(Html, Opts), {{html, Html, Opts}, Bindings}).
-define(H(Html), ?H(Html, #{})).
-define(F(Filename, Opts), {{file, Filename, Opts}, Bindings}).
-define(F(Filename), ?F(Filename, #{})).
