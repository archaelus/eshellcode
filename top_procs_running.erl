
% Shows which kinds of processes are running in the system (grouping by initial_call MFA)
% Useful for analyzing process leaks
%
% Example:
% 
% 1> TopProcsRunning().
% [{{proc_lib,init_p,5},12},
%  {{erlang,apply,2},8},
%  {{group,server,3},2},
%  {{application_master,start_it,4},1},
%  {{user_drv,server,2},1},
%  {{otp_ring0,start,2},1}]
%
% 2> ProcsGroupBy(current_function).
% [{{gen_server,loop,6},11},
%  {{group,server_loop,3},2},
%  {{application_master,loop_it,4},1},
%  {{user_drv,server_loop,5},1},
%  {{standard_error,server_loop,1},1},
%  {{code_server,loop,1},1},
%  {{global,loop_the_locker,1},1},
%  {{gen_event,fetch_msg,5},1},
%  {{erl_prim_loader,loop,3},1},
%  {{erl_eval,do_apply,6},1},
%  {{init,loop,1},1},
%  {{global,loop_the_registrar,0},1},
%  {{shell,shell_rep,4},1},
%  {{application_master,main_loop,2},1}]

f(ProcsGroupBy).
ProcsGroupBy = fun(Kind) ->
  Map = [proplists:get_value(Kind, process_info(Pid)) || Pid <- processes()],

  Reduce = lists:foldl(fun(Mfa, Acc) ->
    dict:update_counter(Mfa, 1, Acc)
  end, dict:new(), Map),

  lists:sort(fun({_, N1}, {_, N2}) ->
    N2 =< N1
  end, dict:to_list(Reduce))
end.

f(TopProcsRunning).
TopProcsRunning = fun() ->
  ProcsGroupBy(initial_call)
end.
