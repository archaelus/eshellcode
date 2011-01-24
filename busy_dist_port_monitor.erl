%% @copyright Geoff Cant 2010
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Erlang shell code to watch for busy dist port messages
%% @end

f(GL).
GL=group_leader().
f(Loop).
Loop = fun () ->
               erlang:system_monitor(self(), [busy_dist_port]),
               lists:foldl(fun (N, Acc) ->
                                   receive
                                       {monitor, _,_,_} = M ->
                                           io:format(GL, "~p monitor: ~p~n",
                                                     [{calendar:local_time(), M}]),
                                           Acc
                                   after 1000 ->
                                           Acc
                                   end
                           end,
                           [],
                           lists:seq(1,30)),
               io:format(GL, "Monitoring complete.~n", [])
       end.

spawn(Loop).

               
                           
