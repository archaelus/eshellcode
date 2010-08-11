%% @copyright Geoff Cant 2010
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Erlang shell code to evaluate a fun N times in parallel.
%% @end

f(Pareval).
Pareval = fun (N, F) ->
                  rpc:pmap({erlang, apply}, [[]],
                           [ F || _ <- lists:seq(1, N) ])
          end.
