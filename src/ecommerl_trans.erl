-module(ecommerl_trans).

-export([parse_transform/2]).

parse_transform(Forms, _Options) ->
    EElModule = binary_to_atom(parserl:module_suffix("_eel", Forms)),
    parserl_trans:transform(Forms, [
        parserl_trans:insert_attribute("-on_load(compile/0)."),

        parserl_trans:insert_function(
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
            #{eel_module => EElModule}),

        parserl_trans:replace_function(
            "render(Bindings) -> render(Bindings, #{}).",
            #{}, #{rename_original => render_defs}),

        parserl_trans:unexport_function(render_defs),

        parserl_trans:insert_function(
            ["render(Bindings0, Opts) ->",
             "    {_, Bindings} = render_defs(Bindings0),",
             "    _@eel_module:render(Bindings, Opts)."],
            #{eel_module => EElModule}, [export])
    ]).
