%% @copyright Geoff Cant 2010
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc dbg tracer with call/return timing information.
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
                                   || A <- Args ],
                                 ", ")
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
                            io:format(GL, "~n~p:~p:~p ~p ~p:~p(~s)~n--> ~p in ~pus~n",
                                      [H, M, Seconds, Pid, CM, CF, ArgsToList(AList), Val, Elapsed]),
                            lists:delete(CallKey, CallStack);
                        undefined ->
                            io:format(GL, "~n~p:~p:~p ~p ~p:~p/~p --> ~p~n",
                                      [H, M, Seconds, Pid, CM, CF, CA, Val]),
                            CallStack
                    end;
                (Msg, CallStack) ->
                    case Msg of
                        {trace, Pid, 'receive', TMsg} ->
                            [H,M,Seconds] = TS(erlang:now()),
                            io:format(GL, "~n~p:~p:~p ~p < ~p~n",
                                      [H, M, Seconds, Pid, TMsg]);
                        {trace, Pid, 'send', TMsg, ToPid} ->
                            [H,M,Seconds] = TS(erlang:now()),
                            io:format(GL, "~n~p:~p:~p ~p > ~p: ~p~n",
                                      [H, M, Seconds, Pid, ToPid, TMsg]);
                        {trace_ts, Pid, 'receive', TMsg, Timestamp} ->
                            [H,M,Seconds] = TS(Timestamp),
                            io:format(GL, "~n~p:~p:~p ~p < ~p~n",
                                      [H, M, Seconds, Pid, TMsg]);
                        {trace_ts, Pid, 'send', TMsg, ToPid, Timestamp} ->
                            [H,M,Seconds] = TS(Timestamp),
                            io:format(GL, "~n~p:~p:~p ~p > ~p: ~p~n",
                                      [H, M, Seconds, Pid, ToPid, TMsg]);
                        _Else ->
                            Timestamp = case element(tuple_size(Msg), Msg) of
                                            {Mg,S,MS} when is_integer(Mg),
                                                           is_integer(S),
                                                           is_integer(MS) ->
                                                {Mg, S, MS};
                                            _Other -> erlang:now()
                                        end,
                            [H,M,Seconds] = TS(Timestamp),
                            io:format(GL, "~n~p:~p:~p ~p~n",
                                      [H, M, Seconds, Msg])
 
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

[ dbg:n(N) || N <- nodes(known) ].
