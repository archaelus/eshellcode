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
                           
f(TS).
TS = fun (TimeStamp = {_,_,Mics}) ->
             {_,{H, M, Secs}} = calendar:now_to_local_time(TimeStamp),
             Micros = Mics div 1000,
             Seconds = Secs rem 60 + (Micros / 1000),
             [H,M,Seconds];
         (OtherTimestamp) ->
             io:format(GL, "Help, help, got weird ts: ~p~n", [OtherTimestamp]),
             [0,0,0]
     end,
dbg:tracer(process,
           {fun ({trace, Pid, 'call', Call={CM,CF,CA}}, CallStack) ->
                    Now = erlang:now(),
                    [H,M,Seconds] = TS(Now),
                    %%io:format(GL, "~n~p:~p:~p ~p ~p:~p(~s)~n",
                    %%          [H, M, Seconds, Pid, CM, CF, ArgsToList(CA)]),
                    [{{Pid, CM, CF, length(CA)}, {CA, Now}} | CallStack];
                ({trace, Pid, return_from, {CM,CF,CA}, Val}, CallStack) ->
                    Now = erlang:now(),
                    CallKey = {Pid, CM, CF, CA},
                    [H,M,Seconds] = TS(Now),
                    case proplists:get_value(CallKey, CallStack) of
                        {AList,CallTS} ->
                            Elapsed = timer:now_diff(Now, CallTS),
                            if Elapsed > 100000 -> % 100 * 1000
                                    io:format(Log, "~n~p:~p:~p ~p ~p:~p(~s)~n--> ~p in ~pus~n",
                                              [H, M, Seconds, Pid, CM, CF, ArgsToList(AList), Val, Elapsed]);
                               true -> ok
                            end,
                            lists:delete(CallKey, CallStack);
                        undefined ->
                            CallStack
                    end;
                (_, CallStack) ->
                    CallStack
            end,
            []}).

f(RetTraceP).
RetTraceP = dbg:fun2ms(fun (_) -> return_trace() end).

f(Stop).
Stop = fun () ->
               dbg:stop_clear(),
               io:format(Log, "~nTrace stopped at ~p.~n", [erlang:now()]),
               file:close(Log)
       end.

io:format(Log, "Trace started on ~p at ~p.~n", [node(), calendar:local()]).

Stop().
