%% @copyright Geoff Cant 2010
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Give a percentage-wise memory breakdown for:
%%  the system - Breakdown(mem).
%%  ets tables - Breakdown(ets).
%%
%% Memory sizes reported in bytes.
%% @end

f(Breakdown).

Breakdown = fun (mem) ->
                    Mem = erlang:memory(),
                    Total = proplists:get_value(total, Mem),
                    [{K, trunc(V*10000 / Total) / 100, V / (1024*1024), V}
                     || {K, V} <- Mem,
                        K =/= total];
                (ets) ->
                    Total = proplists:get_value(ets, erlang:memory()),
                    WordSize = erlang:system_info(wordsize),
                    Mem = [{T, ets:info(T, memory) * WordSize} || T <- ets:all(), is_integer(ets:info(T, memory))],
                    [{K, trunc(V*10000 / Total) / 100, V}
                     || {K, V} <- lists:reverse(lists:keysort(2, Mem))]
            end.
