%% @copyright Geoff Cant 2011
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Convenience recompilation for modules not in cwd.
%% @end

f(Recompile).
Recompile = fun(Mod, ExtraOpts) ->
                    Outdir = filename:dirname(code:which(Mod)),
                    Compile = Mod:module_info(compile),
                    OldTime = proplists:get_value(time, Compile),
                    Opts = proplists:get_value(options, Compile),
                    c:c(proplists:get_value(source, Compile),
                        ExtraOpts ++ [{outdir, Outdir}] ++ Opts)
            end.
