-module(parserl_trans).

%% API
-export([transform/2, insert_above/1, insert_below/1, insert_attribute/1,
         insert_attribute/2, remove_attribute/1, insert_function/1,
         insert_function/2, insert_function/3, replace_function/1,
         replace_function/2, replace_function/3, export_function/2,
         unexport_function/1, unexport_function/2, debug/0, write_file/1,
         maybe/2]).

%%%=============================================================================
%%% API
%%%=============================================================================

transform(Forms, TransFuns) ->
    parserl:foldl(TransFuns ++ [revert()], Forms).

insert_above(Form) ->
    fun(Forms) -> parserl:insert_above(Form, Forms) end.

insert_below(Form) ->
    fun(Forms) -> parserl:insert_below(Form, Forms) end.

insert_attribute(Text) ->
    insert_attribute(Text, []).

insert_attribute(Text, Env) ->
    fun(Forms) -> parserl:insert_attribute(Text, Env, Forms) end.

remove_attribute(Name) ->
    fun(Forms) -> parserl:remove_attribute(Name, Forms) end.

insert_function(Text) ->
    insert_function(Text, []).

insert_function(Text, Env) ->
    insert_function(Text, Env, []).

insert_function(Text, Env, Opts) ->
    fun(Forms) -> parserl:insert_function(Text, Env, Forms, Opts) end.

replace_function(Text) ->
    replace_function(Text, []).

replace_function(Text, Env) ->
    replace_function(Text, Env, []).

replace_function(Text, Env, Opts) ->
    fun(Forms) -> parserl:replace_function(Text, Env, Forms, Opts) end.

export_function(Name, Arity) ->
    fun(Forms) -> parserl:export_function(Name, Arity, Forms) end.

unexport_function(Name) ->
    fun(Forms) -> parserl:unexport_function(Name, Forms) end.

unexport_function(Name, Arity) ->
    fun(Forms) -> parserl:unexport_function(Name, Arity, Forms) end.

debug() ->
    fun(Forms) -> parserl:debug(Forms) end.

write_file(Filename) ->
    fun(Forms) -> parserl:write_file(Filename, Forms) end.

maybe(true, TransFun) ->
    TransFun;
maybe(false, _) ->
    fun(Forms) -> Forms end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

revert() ->
    fun(Forms) -> parserl:revert(Forms) end.
