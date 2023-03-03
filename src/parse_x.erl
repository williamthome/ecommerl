-module(parse_x).

-export([parse_transform/3, parse_transform/5, get_context/1, set_context/2,
         append_attrs/1, append_attrs/2, append_funs/1, append_funs/2,
         maybe_append_funs/1, maybe_append_funs/2, fold/2, print_result/1,
         find_function/3, normalize_forms/1]).

-export_type([form/0, body/0, insp_f/0]).

-include_lib("syntax_tools/include/merl.hrl").

% Copy of parse_trans context
-record(context, {
    module,
    function,
    arity,
    file,
    options
}).

-record(attribute, {
    body :: body(),
    form :: form()
}).

-record(function, {
    name  :: atom(),
    arity :: non_neg_integer(),
    body  :: body(),
    form  :: form(),
    public = true :: boolean()
}).

-record(transform, {
    attributes = [] :: [#attribute{}],
    functions  = [] :: [#function{}]
}).

-record(state, {
    icontext  :: term(),
    context   :: #context{},
    transform :: #transform{},
    forms     :: list()
}).

-type form() :: erl_syntax:syntaxTree().
-type body() :: string() | [string()].
% Copy from parse_trans (types not exported)
-type type()   :: atom().
-type insp_f() :: fun((type(), form(), #context{}, A) -> {boolean(), A}).

%%%=============================================================================
%%% API
%%%=============================================================================

parse_transform(Forms, Options, TransFuns) ->
    parse_transform(Forms, Options, fun inspect/4, [], TransFuns).

parse_transform(Forms, Options, Inspect, InspectContext, TransFuns)
  when is_function(Inspect, 4),
       is_list(TransFuns) ->
    Context = parse_trans:initial_context(Forms, Options),
    InitialState = #state{icontext  = InspectContext,
                          context   = Context,
                          forms     = Forms,
                          transform = #transform{}},
    State = parse_trans:do_inspect(Inspect, InitialState, Forms, Context),
    parse_trans:return(result(State, TransFuns), Context).

get_context(#state{icontext = Context}) ->
    Context.

set_context(Context, #state{} = State) ->
    State#state{context = Context}.

append_attrs(Attrs) ->
    fun(State) -> append_attrs(State, Attrs, []) end.

append_attrs(Attrs, Env) ->
    fun(State) -> append_attrs(State, Attrs, Env) end.

append_funs(Funs) ->
    fun(State) -> append_funs(State, Funs, []) end.

append_funs(Funs, Env) ->
    fun(State) -> append_funs(State, Funs, Env) end.

maybe_append_funs(Funs) ->
    fun(State) -> maybe_append_funs(State, Funs, []) end.

maybe_append_funs(Funs, Env) ->
    fun(State) -> maybe_append_funs(State, Funs, Env) end.

print_result(Forms) ->
    io:format("~s~n", [lists:flatten(
        [erl_pp:form(F) || F <- epp:restore_typed_record_fields(Forms)]
    )]),
    Forms.

find_function(FunName, Arity, [{function, _, FunName, Arity, _} = Fun | _]) ->
    {true, Fun};
find_function(FunName, Arity, [_ | T]) ->
    find_function(FunName, Arity, T);
find_function(_, _, []) ->
    false.

normalize_forms(Forms) when is_list(Forms) ->
    epp:restore_typed_record_fields(
        [erl_syntax:revert(T) || T <- lists:flatten(Forms)]);
normalize_forms(Forms) when is_tuple(Forms) ->
    normalize_forms([Forms]).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

inspect(_, _, _, State) ->
    {false, State}.

append_attr(#state{transform = #transform{attributes = Attrs} = Trans} = State,
            #attribute{} = Attr) ->
    State#state{transform = Trans#transform{attributes = [Attr | Attrs]}};
append_attr(State, Text) ->
    append_attr(State, Text, []).

append_attr(State, Text, Env) ->
    Body = flatten_text(Text),
    Form = text_to_form(Text, Env),
    append_attr(State, #attribute{body = Body, form = Form}).

append_attrs(State, Attrs, Env) ->
    lists:foldl(fun
                    ({A, E}, S) when is_function(E, 0); is_list(E) ->
                        append_attr(S, A, [E | Env]);
                    (A, S) ->
                        append_attr(S, A, Env) end,
                State,
                Attrs).

append_fun(#state{transform = #transform{functions = Funs} = Trans} = State,
           #function{} = Fun) ->
    State#state{transform = Trans#transform{functions = [Fun | Funs]}};
append_fun(State, Text) ->
    append_fun(State, Text, [], #{}).

append_fun(State, Text, Env) when is_list(Env) ->
    append_fun(State, Text, Env, #{});
append_fun(State, Text, Opts) when is_map(Opts) ->
    append_fun(State, Text, [], Opts).

append_fun(State, Text, Env, Opts) ->
    append_fun(State, text_to_fun(Text, Env, Opts)).

append_funs(State, Funs, Env) ->
    lists:foldl(fun
                    ({F, E}, S) when is_function(E, 0); is_list(E) ->
                        append_fun(S, F, [E | Env]);
                    ({F, O}, S) when is_map(O) ->
                        append_fun(S, F, Env, O);
                    ({F, E, O}, S) when (is_function(E, 0) orelse is_list(E)), is_map(O) ->
                        append_fun(S, F, [E | Env], O);
                    (F, S) ->
                        append_fun(S, F, Env) end,
                State,
                Funs).

maybe_append_fun(State, Bool, Fun, Env) ->
    maybe_append_fun(State, Bool, Fun, Env, #{}).

maybe_append_fun(State, false, _, _, _) ->
    State;
maybe_append_fun(State, undefined, _, _, _) ->
    State;
maybe_append_fun(State, _, Fun, Env, Opts) ->
    append_fun(State, Fun, Env, Opts).

maybe_append_funs(State, Funs, Env) ->
    lists:foldl(fun({Bool, F}, S) ->
                       maybe_append_fun(S, Bool, F, Env);
                   ({Bool, F, E}, S) when is_function(E, 0) ->
                       maybe_append_fun(S, Bool, F, [E | Env]);
                   ({Bool, F, E, O}, S) when is_function(E, 0), is_map(O) ->
                       maybe_append_fun(S, Bool, F, [E | Env], O);
                   ({Bool, F, O}, S) when is_map(O) ->
                       maybe_append_fun(S, Bool, F, Env, O) end,
                State,
                Funs).

fold(State, Funs) ->
    lists:foldl(fun(F, S) -> F(S) end, State, Funs).

result(State, TransformFuns) ->
    #state{forms = Forms} = normalize_state(transform(fold(State, TransformFuns))),
    Forms.

transform(State) ->
    fold(State, [fun attrs_to_forms/1,
                 fun funs_to_forms/1]).
attrs_to_forms(#state{transform = #transform{attributes = Attrs},
                      forms = InitForms,
                      context = Context} = State) ->
    Forms =
        lists:foldl(
            fun(#attribute{form = Form}, Acc) ->
                parse_trans:do_insert_forms(below, [Form], Acc, Context)
            end,
            InitForms,
            lists:reverse(Attrs)
        ),
    State#state{forms = Forms}.

funs_to_forms(#state{transform = #transform{functions = Funs},
                     forms = InitForms,
                     context = Context} = State) ->
    Forms =
        lists:foldl(
            fun(#function{name = Name, arity = Arity, form = Form, public = Public}, Acc) ->
                case parse_trans:function_exists(Name, Arity, InitForms) of
                    true ->
                        %% TODO: Change ending function from dot to comma and
                        %%       insert code below the existing function.
                        parse_trans:do_insert_forms(below, [Form], Acc, Context);

                    false ->
                        case Public of
                            true ->
                                parse_trans:export_function(Name, Arity,
                                    parse_trans:do_insert_forms(below, [Form], Acc, Context));

                            false ->
                                parse_trans:do_insert_forms(below, [Form], Acc, Context)
                        end
                end
            end,
            InitForms,
            lists:reverse(Funs)
        ),
    State#state{forms = Forms}.

to_merl_term(Env) ->
    to_merl_term(Env, []).

to_merl_term(Env, Acc0) ->
    lists:foldl(fun({{erl_syntax, K}, V}, Acc1) -> [{K, V} | Acc1];
                    ({K, V}, Acc1) -> [{K, merl:term(V)} | Acc1];
                    (F, Acc1) when is_function(F, 0) -> to_merl_term(F(), Acc1);
                    (E, Acc1) when is_list(E) -> to_merl_term(E, Acc1) end,
                Acc0,
                Env).

normalize_state(#state{forms = Forms} = State) ->
    State#state{forms = normalize_forms(Forms)}.

text_to_form(Text, Env0) ->
    Body = flatten_text(Text),
    Env = to_merl_term(Env0),
    case Env =:= [] of
        true -> try ?Q(Text)
                catch _:Reason:Stack ->
                    error({error, {text_to_form, {text, Text}, Env, Reason, Stack}}) end;
        false -> try ?Q(Body, Env)
                 catch _:Reason:Stack ->
                     error({error, {text_to_form, {body, Body}, Env, Reason, Stack}}) end
    end.

text_to_fun(Text, Env, Opts) when is_list(Text); is_binary(Text) ->
    Body = flatten_text(Text),
    Name = guess_fun_name(Body),
    Arity = guess_fun_arity(Body),
    Form = text_to_form(Text, Env),
    Public = maps:get(public, Opts, true),
    #function{name = Name, arity = Arity, body = Body, form = Form, public = Public}.

guess_fun_name(Body) ->
    guess_fun_name(Body, []).

guess_fun_name([$( | _], Acc) ->
    erlang:list_to_atom(lists:reverse(Acc));
guess_fun_name([32 | T], []) ->
    guess_fun_name(T, []);
guess_fun_name([H | T], Acc) ->
    guess_fun_name(T, [H | Acc]).

guess_fun_arity(Body) ->
    guess_fun_arity(Body, 0, 0).

guess_fun_arity([$) | _], 1, Arity) ->
    Arity;
guess_fun_arity([$( | T], Depth, 0) ->
    case maybe_arity_zero(T) of
        true -> 0;
        false -> guess_fun_arity(T, Depth + 1, 1)
    end;
guess_fun_arity([$( | T], Depth, Arity) ->
    guess_fun_arity(T, Depth + 1, Arity);
guess_fun_arity([$# | T], Depth, Arity) ->
    guess_fun_arity(T, Depth + 1, Arity);
guess_fun_arity([$} | T], Depth, Arity) ->
    guess_fun_arity(T, Depth - 1, Arity);
guess_fun_arity([$[ | T], Depth, Arity) ->
    guess_fun_arity(T, Depth + 1, Arity);
guess_fun_arity([$] | T], Depth, Arity) ->
    guess_fun_arity(T, Depth - 1, Arity);
guess_fun_arity([$, | T], 1, Arity) ->
    guess_fun_arity(T, 1, Arity + 1);
guess_fun_arity([_ | T], Depth, Arity) ->
    guess_fun_arity(T, Depth, Arity).

maybe_arity_zero([32 | T]) ->
    maybe_arity_zero(T);
maybe_arity_zero([$) | _]) ->
    true;
maybe_arity_zero(_) ->
    false.

flatten_text([L | _] = Lines) when is_list(L) ->
    lists:foldr(fun(S, T) -> S ++ [$\n | T] end, "", Lines);
flatten_text([B | _] = Lines) when is_binary(B) ->
    lists:foldr(fun(S, T) -> binary_to_list(S) ++ [$\n | T] end, "", Lines);
flatten_text(Text) when is_binary(Text) ->
    binary_to_list(Text);
flatten_text(Text) ->
    Text.
