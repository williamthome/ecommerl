-module(ecommerl_trans).
-compile(nowarn_unused_function).

-export([parse_transform/2]).

-include_lib("syntax_tools/include/merl.hrl").

parse_transform(Forms0, _Options) ->
    Module = parse_trans:get_module(Forms0),
    EElModule = binary_to_atom(<<(atom_to_binary(Module))/binary, "_eel">>),
    Forms1 = parse_x:replace_function("render(Bindings) -> render(Bindings, #{}).",
                                      [{eel_module, EElModule}],
                                      Forms0,
                                      [{rename_original, render_defs}]),
    Forms2 = parse_x:insert_function(["render(Bindings0, Opts) ->",
                                       "    {_, Bindings} = render_defs(Bindings0),",
                                       "    _@eel_module:render(Bindings, Opts)."],
                                      [{eel_module, EElModule}], Forms1),
    Forms3 = parse_x:unexport_function(render_defs, 1, Forms2),
    Forms4 = parse_x:insert_attribute("-on_load(compile/0).", Forms3),
    Forms5 = parse_x:insert_function(
        ["compile() ->",
         "    {ok, _@eel_module} =",
         "        case render_defs(#{}) of",
         "            {{html, Html0}, _} ->",
         "                Html = unicode:characters_to_nfc_binary(string:trim(Html0)),"
         "                eel:compile_to_module(Html, _@eel_module);",
         "            {{html, Html0, Opts}, _} ->",
         "                Html = unicode:characters_to_nfc_binary(string:trim(Html0)),"
         "                eel:compile_to_module(Html, _@eel_module, Opts);",
         "            {{file, Filename}, _} ->",
         "                eel:compile_file_to_module(Filename, _@eel_module);",
         "            {{file, Filename, Opts}, _} ->",
         "                eel:compile_file_to_module(Filename, _@eel_module, Opts)",
         "        end,",
         "    ok."],
        [{eel_module, EElModule}], Forms4),
    % parse_x:pprint(
        parse_x:normalize_forms(Forms5)
    % )
    .
