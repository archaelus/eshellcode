%% @copyright Geoff Cant 2010
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Erlang shell code to trace process messages to a wrap log.
%% @end

dbg:stop_clear().
f(GL).
GL=group_leader().
f(Log).
{ok, Log} = file:open("dbg_trace.txt",
                      [write,
                       delayed_write]).
f(ArgsToList).
ArgsToList = fun (Args) ->
                     string:join([ if is_pid(A) ->
                                           "pid("++string:join(string:tokens(pid_to_list(A) -- "<>", "."), ",") ++ ")";
                                      true ->
                                           io_lib:format("~p", [A])
                                   end
                                   || A <- Args ], ", ")
             end.

f(LogFmt).
LogFmt = fun (Log, {_,_,Mics} = TS, Fmt, Args) ->
                 {_,{H, M, Secs}} = calendar:now_to_local_time(TS),
                 Micros = Mics div 1000,
                 Seconds = Secs rem 60 + (Micros / 1000),
                 io:format(Log, "~n~2.2.0w:~2.2.0w:~6.3.0f " ++ Fmt ++ "~n",
                           [H,M,Seconds] ++ Args)
         end.

dbg:tracer(process,
           {fun (T, CallStack) ->
                    {Now, Trace} = case element(1,T) of
                                       trace ->
                                           {erlang:now(), list_to_tuple(tl(tuple_to_list(T)))};
                                       trace_ts ->
                                           {element(tuple_size(T), T),
                                            list_to_tuple(string:sub_string(tuple_to_list(T),2, tuple_size(T) - 1))};
                                       _ -> {erlang:now(), T}
                                   end,
                    case Trace of
                        {Pid, 'call', {CM,CF,CA}} ->
                            LogFmt(Log, Now, "~p ~p:~p(~s)~n",
                                   [Pid, CM, CF, ArgsToList(CA)]);
                        {Pid, 'return_from', {CM,CF,CA}, Val} ->
                            LogFmt(Log, Now, "~p ~p:~p/~p -->~n~p~n",
                                      [Pid, CM, CF, CA, Val]);
                        {Pid, 'receive', TMsg} ->
                            LogFmt(Log, Now, "~p < ~p~n",
                                   [Pid, TMsg]);
                        {Pid, 'send', TMsg, ToPid} ->
                            LogFmt(Log, Now, "~p > ~p : ~p~n",
                                   [Pid, ToPid, TMsg]);
                        Else ->
                            LogFmt(Log, Now, "~p~n",
                                   [Else])
                    end,
                    CallStack
            end,
            []}).


f(RetTraceP).
RetTraceP = dbg:fun2ms(fun (_) -> return_trace() end).

f(Stop).
Stop = fun () ->
               dbg:stop_clear(),
               LogFmt(Log, erlang:now(), "~nTrace stopped.~n", []),
               file:close(Log)
       end.

LogFmt(Log, erlang:now(),
       "Trace started from ~p.~n", [node()]).

[dbg:n(N) || N <- nodes()].

Stop().
