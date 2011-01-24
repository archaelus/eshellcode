%% @copyright 2011 Geoff Cant
%% @author Geoff Cant <gcant@erlang.geek.nz>
%% @version {@date} {@time}
%% @doc Function call timing analysis. Parts lifted from hipe_timer.erl (thanks OTP)
%% @end
-module(funalysis).

-export([advanced/2,
         par_advanced/3]).
-export([par_proc_iterations/2,
         analyse/2]).

t(F) ->
    NullTime = empty_time(),
    {Time, _} = timer:tc(F, []),
    erlang:max(Time - NullTime, 0).

% -spec empty_time() -> microseconds().
empty_time() ->
    {Time, _} = timer:tc(fun () -> ok end, []),
    Time.

advanced(_Fun, I) when I < 2 -> false;
advanced(Fun, Iterations) ->
    Measurements = [t(Fun) || _ <- lists:seq(1, Iterations)],
    analyse(Measurements, Iterations).

par_advanced(Fun, Procs, Iterations) when Iterations > Procs, Procs > 0 ->
    Master = self(),
    Ref = make_ref(),
    Pids = [ spawn( fun () -> timing_loop(Master, Ref, Fun, ProcIters ) end )
             || ProcIters <- par_proc_iterations(Procs, Iterations) ],
    Measurements = lists:flatmap( fun (Pid) ->
                                          receive
                                              {Pid, Ref, M} -> M
                                          end
                                  end,
                                  Pids),
    analyse(Measurements, Iterations).

par_proc_iterations(Procs, Iterations) ->
    Rem = (Iterations rem Procs),
    [ case N =< Rem of
          true -> (Iterations div Procs) + 1;
          false -> (Iterations div Procs)
      end
      || N <- lists:seq(1, Procs) ].

timing_loop(Master, Ref, Fun, ProcIters) ->
    Master ! {self(), Ref, [t(Fun) || _ <- lists:seq(1, ProcIters) ]},
    exit(normal).

analyse(Measurements, Iterations) ->
  Wallclock = Measurements,
  WMin = lists:min(Wallclock),
  WMax = lists:max(Wallclock),
  WMean = mean(Wallclock),
  WMedian = median(Wallclock),
  WVariance = variance(Wallclock),
  WStddev = stddev(Wallclock),
  WVarCoff = 100 * WStddev / WMean,
  WSum = lists:sum(Wallclock),
  [{wallclock,[{min, WMin},
	       {max, WMax},
	       {mean, WMean},
	       {median, WMedian},
	       {variance, WVariance},
	       {stdev, WStddev},
	       {varcoff, WVarCoff},
	       {sum, WSum},
	       {values, Wallclock}]},
   {iterations, Iterations}].

split(M) -> 
  split(M, [], []).

split([{W,R}|More], AccW, AccR) ->
  split(More, [W|AccW], [R|AccR]);
split([], AccW, AccR) ->
  {AccW, AccR}.

mean(L) ->
  mean(L, 0, 0).

mean([V|Vs], No, Sum) ->
  mean(Vs, No+1, Sum+V);
mean([], No, Sum) when No > 0 ->
  Sum/No;
mean([], _No, _Sum) ->
  exit(empty_list).
  
median(L) ->
  S = length(L),
  SL = lists:sort(L),
  case even(S) of
    true ->
      (lists:nth((S div 2), SL) + lists:nth((S div 2) + 1, SL)) / 2;
    false ->
      lists:nth((S div 2), SL)
  end.

even(S) ->
  (S band 1) =:= 0.

%% diffs(L, V) ->
%%   [X - V || X <- L].

square_diffs(L, V) ->
  [(X - V) * (X - V) || X <- L].

variance(L) ->
  Mean = mean(L),
  N = length(L),
  if N > 1 ->
      lists:sum(square_diffs(L,Mean)) / (N-1);
     true -> exit('too few values')
  end.

stddev(L) ->
  math:sqrt(variance(L)).
