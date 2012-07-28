f(GL).
GL=group_leader().

TablePid ! exit.
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
Threshold = 200 * 1000. %% 25ms

f(ArgsToList).
ArgsToList = fun (Args) ->
                     string:join([ if is_pid(A) ->
                                           "pid("++string:join(string:tokens(pid_to_list(A) -- "<>", "."), ",") ++ ")";
                                      true ->
                                           io_lib:format("~p", [A])
                                   end
                                   || A <- Args ], ", ")
             end.
f(TS).
TS = fun (TimeStamp = {_,_,Mics}) ->
             {_,{H, M, Secs}} = calendar:now_to_local_time(TimeStamp),
             Micros = Mics div 1000,
             Seconds = Secs rem 60 + (Micros / 1000),
             [H,M,(Secs + Mics / 1000000)];
         (OtherTimestamp) ->
             [0,0,0]
     end.

f(DbgF).
DbgF =
fun (Msg, Acc) ->
        try
        case tuple_to_list(Msg) of
            [trace_ts, Pid, Type | Rest] ->
                {TraceInfo, [Timestamp]} = lists:split(length(Rest)-1, Rest);
            [trace, Pid, Type | Rest] ->
                TraceInfo = Rest,
                Timestamp = os:timestamp()
        end,
        [Hours, Mins, Secs] = TS(Timestamp),
        case Type of
            %% {trace, Pid, 'receive', Msg}
            'receive' ->
                [PMsg] = TraceInfo,
                catch ets:insert(time_tracer, {{Pid, 'receive'}, Timestamp, PMsg});
            %% {trace, Pid, in, {M, F, Arity} | 0}
            in ->
                case TraceInfo of
                    [{M, F, Arity}] ->
                        %% {"scheduled in for ~p:~p/~p",
                        %%  [M, F, Arity]};
                        catch ets:insert(time_tracer, {{Pid, in}, Timestamp, {M, F, Arity}});
                    [0] ->
                        catch ets:insert(time_tracer, {{Pid, in}, Timestamp, {unknown, function, arity}})
                end;
            %% {trace, Pid, out, {M, F, Arity} | 0}
            out ->
                NewMFA = case TraceInfo of
                             [{M, F, Arity}] -> {M,F,Arity};
                             _ -> {unknown, function, arity}
                         end,
                LastMsg = case ets:lookup(time_tracer, {Pid, 'receive'}) of
                              [{_, _Rts, Rmsg}] ->
                                  Rmsg;
                              [] -> no_msg
                          end,
                case ets:lookup(time_tracer, {Pid, in}) of
                    [{{Pid, in}, OldTs, OldMFA}] ->
                        case timer:now_diff(Timestamp, OldTs) of
                            Elapsed when Elapsed > Threshold ->
                                io:format(GL, "~n~p:~p:~9.6.0f spend ~p us (in:~p, out:~p, last_msg:~p) ~n",
                                          [Hours, Mins, Secs, Pid, Elapsed, OldMFA, NewMFA, LastMsg]);
                            _ ->
                                ok
                        end;
                    _ -> ok
                end,
                ets:delete(time_tracer, {Pid, in});
            %% {trace, Pid, gc_start, Info}
            gc_start ->
                [Info] = TraceInfo,
                HeapSize = proplists:get_value(heap_size, Info),
                {"gc beginning -- heap ~p bytes", [HeapSize]};
            %% {trace, Pid, gc_end, Info}
            gc_end ->
                [Info] = TraceInfo,
                HeapSize = proplists:get_value(heap_size, Info),
                OldHeapSize = proplists:get_value(old_heap_size, Info),
                {"gc finished -- heap ~p bytes (recovered ~p bytes)",
                 [HeapSize, OldHeapSize-HeapSize]};
            _ -> ok
        end
        %% io:format(GL, "~n~p:~p:~9.6.0f ~p ~n",
        %%           [Hours, Mins, Secs, Pid] ++ Args)
        catch Class:Error ->
        io:format(GL, "~nDied ~p:~p (~p)~n",
                  [Class,Error, Msg]),
                    exit(Error)
        end,
        Acc
end.

dbg:stop_clear().
dbg:tracer(process, {DbgF, group_leader()}).

f(RetTraceP).
RetTraceP = dbg:fun2ms(fun (_) -> return_trace() end).

f(DbgProc).
DbgProc = fun (Pid, Flags) ->
                  dbg:p(Pid, Flags),
                  {links, Links} = process_info(Pid, links),
                  [ dbg:p(L, Flags) || L <- Links,
                                       is_pid(L),
                                       L > Pid]
          end.
