%% @copyright Geoff Cant 2009, 2010
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Better dbg tracer process.
%% @end

dbg:stop_clear().
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
             [H,M,Seconds];
         (OtherTimestamp) ->
             io:format("Help, help, got weird ts: ~p~n", [OtherTimestamp]),
             [0,0,0]
     end,
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
                    [H,M,S] = TS(Now),
                    case Trace of
                        {Pid, 'call', {CM,CF,CA}} ->
                            io:format(GL, "~n~2.2.0w:~2.2.0w:~6.3.0f ~p ~p:~p(~s)~n",
                                      [H,M,S, Pid, CM, CF, ArgsToList(CA)]);
                        {Pid, 'return_from', {CM,CF,CA}, Val} ->
                            io:format(GL, "~n~2.2.0w:~2.2.0w:~6.3.0f ~p ~p:~p/~p -->~n~p~n",
                                      [H,M,S, Pid, CM, CF, CA, Val]);
                        {Pid, 'receive', TMsg} ->
                            io:format(GL, "~n~2.2.0w:~2.2.0w:~6.3.0f ~p < ~p~n",
                                      [H,M,S, Pid, TMsg]);
                        {Pid, 'send', TMsg, ToPid} ->
                            io:format(GL, "~n~2.2.0w:~2.2.0w:~6.3.0f ~p > ~p : ~p~n",
                                      [H,M,S, Pid, ToPid, TMsg]);
                        Else ->
                            io:format(GL, "~n~2.2.0w:~2.2.0w:~6.3.0f ~p~n",
                                      [H,M,S, Else])
                    end,
                    CallStack
            end,
            []}).

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
                   
