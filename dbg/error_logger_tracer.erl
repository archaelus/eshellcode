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
     end.
f(FmtErrMsg).
FmtErrMsg = fun ({notify, {info_msg, PGL, {Proc, Fmt, Args}}}, Time) ->
                    {"~n~s ~p INFO:~n" ++ Fmt, [Time, Proc | Args]};
                ({notify, {info_report, PGL, {Proc, std_info, Args}}}, Time) ->
                    {"~n~s ~p INFO:~n~p~n", [Time, Proc, Args]};
                ({notify, {error, PGL, {Proc, Fmt, Args}}}, Time) ->
                    {"~n~s ~p INFO:~n" ++ Fmt, [Time, Proc | Args]};
                ({notify, {error_report, PGL, {Proc, std_error, Args}}}, Time) ->
                    {"~n~s ~p INFO:~n~p~n", [Time, Proc, Args]}
            end.
dbg:tracer(process,
           {fun ({trace, _Pid, 'receive', {notify, _}=TMsg}, _) ->
                    [H,M,Seconds] = TS(erlang:now()),
                    Time = io_lib:format("~p:~p:~p", [H,M,Seconds]),
                    {Fmt,Args} = FmtErrMsg(TMsg, Time),
                    catch io:format(GL, Fmt, Args),
                    undefined;
                ({trace_ts, _Pid, 'receive', {notify, _}=TMsg, Timestamp}, _) ->
                    [H,M,Seconds] = TS(Timestamp),
                    Time = io_lib:format("~p:~p:~p", [H,M,Seconds]),
                    {Fmt,Args} = FmtErrMsg(TMsg, Time),
                    catch io:format(GL, Fmt, Args),
                    undefined;
                (_Else, _) ->
                    undefined
            end,
            undefined}).

f(RetTraceP).
RetTraceP = dbg:fun2ms(fun (_) -> return_trace() end).
