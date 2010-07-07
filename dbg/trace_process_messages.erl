%% @copyright Geoff Cant 2009, 2010
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Erlang shell code to trace the messages for one process.
%% @end
 
dbg:stop_clear().
f(GL).
GL=group_leader().
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
           {fun (Msg, State) ->
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
                            io:format(GL, "~n~p:~p:~pt ~p < ~p~n",
                                      [H, M, Seconds, Pid, TMsg]);
                        {trace_ts, Pid, 'send', TMsg, ToPid, Timestamp} ->
                            [H,M,Seconds] = TS(Timestamp),
                            io:format(GL, "~n~p:~p:~pt ~p > ~p: ~p~n",
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
                    State
            end,
            undefined}).
 
f(Trace).
Trace = fun(Pid) -> dbg:p(Pid, [m]) end.
