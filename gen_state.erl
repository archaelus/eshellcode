f(State).
State = fun (Proc) ->
                {status, _Pid, _, Things} = sys:get_status(Proc),
                {data, PL} = lists:last(lists:nth(5, Things)),
                proplists:get_value("State", PL)
        end.

f(FsmState).
FsmState = fun (Proc) ->
                {status, _Pid, _, Things} = sys:get_status(Proc),
                {data, PL} = lists:last(lists:nth(5, Things)),
                proplists:get_value("StateData", PL)
        end.

f(FsmStateName).
FsmStateName = fun (Proc) ->
                       {status, _Pid, _, Things} = sys:get_status(Proc),
                       {data, PL} = lists:nth(2, lists:nth(5, Things)),
                       proplists:get_value("StateName", PL)
               end.

