
f(BT).
BT = fun (Name) ->
             Pid = if is_pid(Name) ->
                           Name;
                      true -> whereis(Name)
                   end,
             {backtrace, B} = process_info(Pid, backtrace),
             io:format("Backtrace for ~p:~n~s~n", [Name, B])
     end.
                       
