%% @copyright Geoff Cant 2010
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Erlang shell code to hot upgrade a gen_server module.
%% a proplist.
%% @end

f(Upgrade).
Upgrade = fun (Pids, Module) ->
                  [ sys:suspend(P) || P <- Pids],
                  l(Module),
                  [ begin
                        sys:change_code(P, Module, undefined, undefined),
                        sys:resume(P)
                    end || P <- Pids ],
                  code:soft_purge(Module)
            end.

f(Upgrade2).
Upgrade2 = fun (Pids, Module, Extra) ->
                  [ sys:suspend(P) || P <- Pids],
                  l(Module),
                  [ begin
                        sys:change_code(P, Module, undefined, Extra),
                        sys:resume(P)
                    end || P <- Pids ],
                  code:soft_purge(Module)
            end.