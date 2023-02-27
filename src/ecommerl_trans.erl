-module(ecommerl_trans).
-compile(nowarn_unused_function).

-export([parse_transform/2]).

-include_lib("syntax_tools/include/merl.hrl").

%% TODO: Export mount/2 if not exists

parse_transform(Forms, _Options) ->
    transform(Forms, []).

transform([{function, _, render, _, _} = Forms0 | T], Acc) ->
    Forms = render_fun_forms(Forms0),
    transform(T, [Forms | Acc]);
transform([H | T], Acc) ->
    transform(T, [H | Acc]);
transform([], Acc) ->
    lists:reverse(Acc).

render_fun_forms({function, FunPos, render, 1,
                      [{clause, ClausePos, Args, Guards, FunForms0}]}) ->
    [H | T] = lists:reverse(FunForms0),
    AST = get_ast(H),
    Opts = #{capitalize => true},
    SubsForms = normalize_forms(?Q(
        ["AST = _@ast,",
         "Opts = _@opts,",
         "eel_renderer:render(AST, Bindings, Opts)"],
        [{ast, merl:term(AST)},
         {opts, merl:term(Opts)}])),
    FunForms = T ++ SubsForms,
    {function, FunPos, render, 1, [{clause, ClausePos, Args, Guards, FunForms}]}.

get_ast({tuple, _, [{tuple, _, [K, V]}, {var, _, 'Bindings'}]}) ->
    get_ast_1(eval(K), eval(V)).

get_ast_1(html, Html) when is_list(Html); is_binary(Html) ->
    eel:compile(unicode:characters_to_nfc_binary(Html));
get_ast_1(filename, Filename) when is_list(Filename); is_binary(Filename) ->
    eel:compile_file(Filename).

normalize_forms(Forms) ->
    epp:restore_typed_record_fields(
        [erl_syntax:revert(T) || T <- lists:flatten(Forms)]).

% Dev support

eval(Form) ->
    {value, Value, #{}} = erl_eval:exprs([Form], #{}),
    Value.

debug(Forms0) ->
    Forms = epp:restore_typed_record_fields(Forms0),
    [io:format("~s~n", [lists:flatten([erl_pp:form(F) || F <- Forms])])].
