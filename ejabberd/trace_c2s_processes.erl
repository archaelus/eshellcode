%% @copyright Geoff Cant 2009, 2010
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Erlang shell code to trace the messages for one process.
%% @end
 
%% Run the code in my_tracer.erl first.
 
f(Trace).
Trace = fun(Pid) -> [dbg:p(Pid, [m])
                     || {_, Pid, _, _} <- supervisor:which_children(ejabberd_c2s_sup)]
        end.

f(TraceC2SProcs).
TraceC2SProcs = fun (Flags) ->
                        dbg:p(ejabberd_c2s_sup, [sol | Flags])
                end.

f(Mods).
Mods = fun (Dir) ->
               [list_to_atom(filename:basename(B,".beam"))
                || B <- filelib:wildcard(filename:join(Dir, "*.beam"))]
       end.

f(NL).
NL = fun (Mods) -> [ nl(M) || M <- Mods ], abcast end.
