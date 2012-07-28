%% @copyright Geoff Cant 2009, 2010
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Trace call counts to ets.
%% @end
TablePid ! exit.

f().
dbg:stop_clear().

f(TablePid).
TablePid = erlang:spawn(fun () ->
                                ets:new(time_tracer, [public, named_table, set]),
                                erlang:register(time_tracer_ets, self()),
                                error_logger:info_msg("~p created table 'time_tracer'.~n", [self()]),
                                receive 
                                    _ ->error_logger:info_msg("~p shutdown killing the count_tracer table.~n", [self()])
                                end
                        end).

f(Threshold).
Threshold = 25 * 1000. %% 25ms

f(GL). GL = group_leader().
f(ArgsToList).
ArgsToList = fun (Args) ->
                     string:join([ if is_pid(A) ->
                                           "pid("++string:join(string:tokens(pid_to_list(A) -- "<>", "."), ",") ++ ")";
                                      true ->
                                           io_lib:format("~p", [A])
                                   end
                                   || A <- Args ], ", ")
             end.
f(Report).
Report = fun (Start, Pid, {Cm,Cf,Ca}, Args, Val, Elapsed) ->
                 MS = element(3, Start),
                 {_,{H,M,S}} = calendar:now_to_local_time(Start),
                 Secs = S + (MS / 1000000),
                 CallTime = trunc(Elapsed / 1000),
                 io:format(GL, "~n~2.2.0w:~2.2.0w:~6.3.0f (~p) ~p ~pms ~p:~p(~s) -> ~p~n",
                           [H,M,Secs, node(Pid), Pid, CallTime, Cm, Cf, ArgsToList(Args), Val])
         end.

dbg:tracer(process,
           {fun ({trace_ts, Pid, 'call', {CM,CF,CA}, TS}, Count) ->
                    Key = {Pid, {CM,CF, length(CA)}},
                    try
                        ets:insert(time_tracer, {Key, TS, CA})
                    catch
                        _ -> ok
                    end,
                    case Count rem 1000 of
                        0 ->
                            io:format(GL, "~n~p.", [Count]);
                        _ -> ok
                    end,
                    Count+1;
                ({trace_ts, Pid, 'return_from', MFA, Val, TS}, Count) ->
                    Key = {Pid, MFA},
                    try ets:lookup(time_tracer, Key) of
                        [{Key, Start, CA}] ->
                            ets:delete(time_tracer, Key),
                            case timer:now_diff(TS, Start) of
                                Elapsed when Elapsed > Threshold ->
                                    Report(Start, Pid, MFA, CA, Val, Elapsed),
                                    Count;
                                _ ->
                                    Count
                            end;
                        _ -> Count
                    catch
                        _ -> Count
                    end;
                (Msg, Count) ->
                    case Count rem 1000 of
                        0 ->
                            io:format(GL, "~nUnknown dbg message: ~p", [Msg]);
                        _ -> ok
                    end,
                    Count + 1
            end,
            1}).

f(RetTraceP).
RetTraceP = dbg:fun2ms(fun (_) -> return_trace() end).

f(DM).
DM = fun (Mods) ->
             [ dbg:tp(Mod, RetTraceP)
               || Mod <- Mods ],
             ok
     end.

f(DMExcept).
DMExcept = fun (Module, Functions) ->
                   [dbg:tp(Module, F, A, RetTraceP)
                    || {F, A} <- Module:module_info(exports),
                       not lists:member({F,A}, Functions) ]
           end.
