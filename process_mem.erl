%% @copyright Geoff Cant 2010, 2011
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Process memory analysis.
%% @end

f(Info).
Info = fun (P,I) ->
               element(2, process_info(P,I))
       end.

f(TopN).
TopN = fun (N, Metric) ->
               element(1, lists:split(N, lists:reverse(lists:keysort(2, [{P, Info(P, Metric)} || P <- erlang:processes(), is_process_alive(P)]))))
       end.

f(MoreInfo).
MoreInfo = fun (N) ->
                   [{P, (Mem / (1024*1024)), Info(P, message_queue_len), process_info(P, registered_name)}
                    || {P, Mem} <- TopN(N, memory)]
           end.
