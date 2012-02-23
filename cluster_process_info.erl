%% @copyright Geoff Cant 2012
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Retreive TopN pids in a cluster sorted by a process_info metric.
%% @end

f(Info).
Info = fun (P,I) ->
               element(2, process_info(P,I))
       end.

f(TopN).
TopN = fun (N, Metric) ->
               element(1, lists:split(N, lists:reverse(lists:keysort(2, [{P, Info(P, Metric)} || P <- erlang:processes(), is_process_alive(P)]))))
       end.

f(CI).
CI = fun (N, Metric) ->
             element(1, lists:split(N, lists:reverse(lists:keysort(2, lists:append(element(1, rpc:multicall(erlang, apply, [ fun () -> TopN(N, Metric) end, [] ])))))))
     end.
