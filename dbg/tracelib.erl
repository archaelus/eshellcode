%% @copyright Geoff Cant 2012
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @doc Pretty formatting for dbg trace.
%% @end

f(GL).
GL=group_leader().
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
        {Fmt, Args} =
            case Type of
                %% {trace, Pid, 'receive', Msg}
                'receive' ->
                    [PMsg] = TraceInfo,
                    {"< ~p", [PMsg]};
                %% {trace, Pid, send, Msg, To}
                send ->
                    [PMsg, To] = TraceInfo,
                    {" > ~p: ~p", [To, PMsg]};
                %% {trace, Pid, send_to_non_existing_process, Msg, To}
                send_to_non_existing_process ->
                    [PMsg, To] = TraceInfo,
                    {" > (non_existent) ~p: ~p", [To, PMsg]};
                %% {trace, Pid, call, {M, F, Args}}
                call ->
                    [{M,F,Arguments}] = TraceInfo,
                    {"~p:~p(~s)", [M,F,case Arguments of
                                           Arity when is_integer(Arity) ->
                                               integer_to_list(Arity);
                                           _ ->
                                               ArgsToList(Arguments)
                                       end]};
                %% {trace, Pid, return_to, {M, F, Arity}}
                %% {trace, Pid, return_from, {M, F, Arity}, ReturnValue}
                return_from ->
                    [{M, F, Arity}, ReturnValue] = TraceInfo,
                    {"~p:~p/~p --> ~p", [M,F,Arity, ReturnValue]};
                %% {trace, Pid, exception_from, {M, F, Arity}, {Class, Value}}
                exception_from ->
                    [{M,F,Arity}, {C,E}] = TraceInfo,
                    {"~p:~p/~p ~p ~p", [M,F,Arity, C, E]};
                %% {trace, Pid, spawn, Pid2, {M, F, Args}}
                spawn ->
                    [Pid2, {M, F, Arguments}] = TraceInfo,
                    {"spawned ~p as ~p:~p(~s)",
                     [Pid2, M, F, ArgsToList(Arguments)]};
                %% {trace, Pid, exit, Reason}
                exit ->
                    [Reason] = TraceInfo,
                    {"EXIT ~p", [Reason]};
                %% {trace, Pid, link, Pid2}
                link ->
                    [Pid2] = TraceInfo,
                    {"link(~p)", [Pid2]};
                %% {trace, Pid, unlink, Pid2}
                unlink ->
                    [Pid2] = TraceInfo,
                    {"unlink(~p)", [Pid2]};
                %% {trace, Pid, getting_linked, Pid2}
                getting_linked ->
                    [Pid2] = TraceInfo,
                    {"getting linked by ~p", [Pid2]};
                %% {trace, Pid, getting_unlinked, Pid2}
                getting_unlinked ->
                    [Pid2] = TraceInfo,
                    {"getting unlinked by ~p", [Pid2]};
                %% {trace, Pid, register, RegName}
                register ->
                    [Name] = TraceInfo,
                    {"registered as ~p", [Name]};
                %% {trace, Pid, unregister, RegName}
                unregister ->
                    [Name] = TraceInfo,
                    {"no longer registered as ~p", [Name]};
                %% {trace, Pid, in, {M, F, Arity} | 0}
                in ->
                    case TraceInfo of
                        [{M, F, Arity}] ->
                            {"scheduled in for ~p:~p/~p",
                             [M, F, Arity]};
                        [0] ->
                            {"scheduled in", []}
                    end;
                %% {trace, Pid, out, {M, F, Arity} | 0}
                out ->
                    case TraceInfo of
                        [{M, F, Arity}] ->
                            {"scheduled out from ~p:~p/~p",
                             [M, F, Arity]};
                        [0] ->
                            {"scheduled out", []}
                    end;
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
                _ ->
                    {"unknown trace type ~p -- ~p",
                     [Type, TraceInfo]}
            end,
        io:format(GL, "~n~p:~p:~9.6.0f ~p " ++ Fmt ++ "~n",
                  [Hours, Mins, Secs, Pid] ++ Args)
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

{ok, Modules} = application:get_key(hermes, modules),
[dbg:tp(Mod, RetTraceP) || Mod <- Modules].
