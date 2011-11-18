f(GL).
GL = group_leader().
f(Loop).
Loop = fun (F, S) ->
               receive
                   stop -> ok;
                   {ngbs_dispatch, time_eval, {Call,{Start, Elapsed}}} when Elapsed > 10000 ->
                       io:format(GL, "~pms for call ~p.~n", [Elapsed div 1000, Call]),
                       F(F, S);
                   _ -> F(F, S)
               end
       end.
exit(Monitor, kill).
f(Monitor).
Monitor = spawn(fun () ->
                        Loop(Loop, undefined)
                end).

application:set_env(ngbs, time_dispatch, [{report, Monitor}]).
application:set_env(ngbs, time_dispatch, undefined).
