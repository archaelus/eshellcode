%% @copyright Geoff Cant 2010
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Upgrades a gen_server at runtime.
%% @end

f(Upgrade).
Upgrade = fun (Pid, Mod) ->
                  sys:suspend(Pid),
                  code:load_file(Mod),
                  sys:change_code(Pid, Mod, undefined, undefined),
                  sys:resume(Pid),
                  code:purge(Mod)
          end.
