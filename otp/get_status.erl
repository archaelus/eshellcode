%% @copyright Geoff Cant 2009
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Erlang shell code get the state of an OTP compliant process as
%% a proplist.
%% @end

f(GetStatus).
GetStatus = fun (Pid) ->
                    {status,_,
                     Module,
                     [[{'$ancestors',Ancestors},
                       {'$initial_call',MFA}],
                      _,_,Debug,
                      [_,
                       {data, _},
                       {data, [{"State", State}]}]]}
                       = sys:get_status(Pid, timer:seconds(1)),
                    [{state, State},
                     Module,
                     {ancestors, Ancestors},
                     {initial_call, MFA},
                     {debug_flags, Debug}]
            end.

f(GetState).
GetState = fun (Pid) ->
                   proplists:get_value(state, GetStatus(Pid))
           end.
