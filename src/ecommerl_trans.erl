-module(ecommerl_trans).
-compile(nowarn_unused_function).

-export([parse_transform/2]).

-include_lib("syntax_tools/include/merl.hrl").

-record(state, {
    forms = [],
    ast
}).

parse_transform(Forms0, Options) ->
    State = lists:foldr(fun(F, S) -> map(F, S) end, #state{}, Forms0),
    #state{forms = Forms, ast = AST} = State,
    parse_x:parse_transform(Forms, Options, [
        parse_x:maybe_append_funs([
            {
                not parse_trans:function_exists(mount, 2, Forms),
                "mount(_Params, Socket) -> {ok, Socket}."
            },
            {
                not parse_trans:function_exists(handle_params, 2, Forms),
                "handle_params(_Params, Socket) -> {noreply, Socket}."
            }
        ]),
        parse_x:append_funs(["ast() -> _@ast."], [{ast, AST}])
    ]).
    % Forms0.

map({function, FunPos, render, 1,
        [{clause, ClausePos, Args, Guards, FunForms0}]}, State) ->
    [H | T] = lists:reverse(FunForms0),
    AST = get_ast(H),
    Opts = #{capitalize => true},
    SubsForms = parse_x:normalize_forms(?Q(
        ["AST = _@ast,",
         "Opts = _@opts,",
         "eel_renderer:render(AST, Bindings, Opts)"],
        [{ast, merl:term(AST)},
         {opts, merl:term(Opts)}])),
    FunForms = T ++ SubsForms,
    Form = {function, FunPos, render, 1,
               [{clause, ClausePos, Args, Guards, FunForms}]},
    push_form(State#state{ast = AST}, Form);
map(Form, State) ->
    push_form(State, Form).

push_form(#state{forms = Forms} = State, Form) ->
    State#state{forms = [Form | Forms]}.

get_ast({tuple, _, [{tuple, _, [K, V]}, {var, _, 'Bindings'}]}) ->
    get_ast_1(eval(K), eval(V)).

get_ast_1(html, Html) when is_list(Html); is_binary(Html) ->
    eel:compile(unicode:characters_to_nfc_binary(Html));
get_ast_1(filename, Filename) when is_list(Filename); is_binary(Filename) ->
    eel:compile_file(Filename).

eval(Form) ->
    {value, Value, #{}} = erl_eval:exprs([Form], #{}),
    Value.
