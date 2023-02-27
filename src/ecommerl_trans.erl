-module(ecommerl_trans).
-compile(nowarn_unused_function).

-export([parse_transform/2]).

-include_lib("syntax_tools/include/merl.hrl").

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
    Html = get_html(H),
    Tokens = eel_tokenizer:tokenize(unicode:characters_to_nfc_binary(Html)),
    AST = eel_compiler:compile(Tokens),
    Opts = #{capitalize => true},
    SubsForms = normalize_forms(?Q(
        ["AST = _@ast,",
         "Opts = _@opts,",
         "eel_renderer:render(AST, Bindings, Opts)"],
        [{ast, merl:term(AST)},
         {opts, merl:term(Opts)}])),
    FunForms = T ++ SubsForms,
    {function, FunPos, render, 1, [{clause, ClausePos, Args, Guards, FunForms}]}.

get_html({tuple, _, [{string, _, Html}, {var, _, 'Bindings'}]}) -> Html.

normalize_forms(Forms) ->
    epp:restore_typed_record_fields(
        [erl_syntax:revert(T) || T <- lists:flatten(Forms)]).

% Dev support

debug(Forms0) ->
    Forms = epp:restore_typed_record_fields(Forms0),
    [io:format("~s~n", [lists:flatten([erl_pp:form(F) || F <- Forms])])].
