-module(ecommerl_utils).

%% API
-export([encode_json/1, decode_json/1]).
-export([get_csrf_token/0]).

%%%=============================================================================
%%% API
%%%=============================================================================

encode_json(Term) ->
    thoas:encode(Term).

decode_json(JSON) ->
    thoas:decode(JSON).

get_csrf_token() ->
    <<"123456">>.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

% nothing here yet!
