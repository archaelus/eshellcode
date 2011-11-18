-module(ngproc_eventparser).

-export([tracer/1, trace/1, start/1, shutdown/0, drop_table/0, analyze/0,
         format/1]).

start(LeaderPid) ->
    {spawn(fun init/0),
     dbg:tracer(process, tracer(LeaderPid))}.

trace(Nodes) ->
    [dbg:n(N) || N <- Nodes],
    dbg:p(ngproc, [m, timestamp]),
    dbg:tp(ngproc, register, [{'_',[],[{return_trace}]}]),
    dbg:p(all, [c, timestamp]).

init() ->
    ets:new(?MODULE, [bag, public, named_table]),
    erlang:hibernate(?MODULE, shutdown, []).

shutdown() ->
    io:format("Deleting ets table ~p~n", [?MODULE]).

drop_table() ->
    ets:info(ngproc_eventparser, owner) ! normal.

tracer(LeaderPid) when is_pid(LeaderPid) ->
    {fun (Event, State) ->
             case parse(Event, State) of
                 ignore ->
%                     io:format("Ignoring event: ~p~n", [Event]),
                     State;
                 {T, TS, Client, Ref} ->
%                     io:format("Got event ~p: ~p~n", [T, Event]),
                     ets:insert(?MODULE, {pid_to_list(Client),
                                          T, TS,
                                          case Ref of
                                              ref -> ref;
                                              _ -> erlang:ref_to_list(Ref)
                                          end})
             end,
             State
     end,
     {LeaderPid}}.

parse({trace_ts,Client,call,
       {ngproc,register,[_Name,_Pid]},
       TS}, _) ->
    {t0, TS, Client, ref};
parse({trace_ts,LeaderNgproc,'receive',
       {'$gen_sync_all_state_event',
        {Client,Ref},
        {leader_call,local_sync,{register,_Name,_Pid}}},
       TS}, {LeaderNgproc}) ->
    {t3, TS, Client, Ref};
parse({trace_ts,_LocalNgproc,'receive',
       {'$gen_sync_all_state_event',
        {Client,Ref},
        {leader_call,local_sync,{register,_Name,_Pid}}},
       TS}, _) ->
    {t1, TS, Client, Ref};
parse({trace_ts,_LocalNgproc,'receive',
       {'$gen_all_state_event',
        {from_leader,_LeaderNode,
         {register,_Name,_Pid},
         {local_sync_reply,{Client,Ref},ok}}},
       TS}, _) ->
    {t5, TS, Client, Ref};
parse({trace_ts,_LocalNgproc,send,
       {'$gen_sync_all_state_event',
        {Client,Ref},
        {leader_call,local_sync,{register,_Name,_Pid}}},
       {ngproc,_LeaderNode},
       TS}, _) ->
    {t2, TS, Client, Ref};
parse({trace_ts,_LeaderNgproc,send,
       {'$gen_all_state_event',
        {from_leader,_LeaderNode,
         {register,_Name,_Pid},
         {local_sync_reply,{Client,Ref},ok}}},
       {ngproc,_ClientNode},
       TS}, _) ->
    {t4, TS, Client, Ref};
%% t6 is epic lies
parse({trace_ts,_LocalNgproc,send,
       {Ref,ok},Client,TS}, _) ->
    {t6, TS, Client, Ref};
parse({trace_ts,Client,return_from,
       {ngproc,register,2},ok,TS}, _) ->
    {t7, TS, Client, ref};
parse(_, _) ->
    ignore.

format(Analysis) ->
    [io_lib:format("Type,Client,T0,LocalQueue,MasterQueue"
                   ",MasterProcessing,RTT,InterNode~n",[]) |
     [ io_lib:format("~p,'~p',~p,~p,~p,~p,~p,~p~n",
                     [T, C, T0, LQ, MQ, MP, RTT, IN])
       || {C, {T, T0, LQ, MQ, MP, RTT, IN}} <- Analysis] ].

analyze() ->
    dict:to_list(dict:map(fun analyze/2, client_data())).

analyze(_C, Events) ->
    try
        Ref = events_ref(Events),
        case lists:keyfind(t1, 1, Events) of
            false ->
                analyze_local(Ref, Events);
            _ ->
                analyze_remote(Ref, Events)
        end
    catch _:_ -> mystery end.

analyze_local(Ref, Events) ->
    try find_local_events(Ref, Events) of
        {T0, T3, T6, T7} ->
            {local,
             _TimeBucket = clip_to_minute(T0),
             _LocalQueueingTime = undefined,
             _MasterQueueingTime = timer:now_diff(T3, T0),
             _MasterProcessingTime = timer:now_diff(T6, T3),
             _TxnRTT = timer:now_diff(T7, T0),
             _InterNodeDelay = undefined}
    catch _:_ -> mystery end.

analyze_remote(Ref, Events) ->
    try find_remote_events(Ref, Events) of
        {T0, T1, T2, T3, T4, T5, _T6, T7} ->
            {remote,
             _TimeBucket = clip_to_minute(T0),
             _LocalQueueingTime = timer:now_diff(T1, T0),
             _MasterQueueingTime = timer:now_diff(T3, T2),
             _MasterProcessingTime = timer:now_diff(T4, T3),
             _TxnRTT = timer:now_diff(T7, T0),
             _InterNodeDelay = timer:now_diff(T5, T4) - timer:now_diff(T1, T0)}
    catch _:_ -> mystery end.

events_ref(Events) ->
    [{t3, _, Ref}] = [ E || E <- Events, element(1, E) =:= t3 ],
    Ref.

find_local_events(Ref, Events) ->
    {event_ts(t0, ref, Events),
     event_ts(t3, Ref, Events),
     event_ts(t6, Ref, Events),
     event_ts(t7, ref, Events)}.

find_remote_events(Ref, Events) ->
    {event_ts(t0, ref, Events),
     event_ts(t1, Ref, Events),
     event_ts(t2, Ref, Events),
     event_ts(t3, Ref, Events),
     event_ts(t4, Ref, Events),
     event_ts(t5, Ref, Events),
     event_ts(t6, Ref, Events),
     event_ts(t7, ref, Events)}.

event_ts(Type, Ref, Events) ->
    [{Type, TS, Ref}] =
        [E || E = {T, _, R} <- Events,
              T =:= Type,
              R =:= Ref ],
    TS.

clip_to_minute({Mega, S, _}) ->
    1000 * 1000 * Mega + ((S div 60) * 60).

client_data() ->
    lists:foldl(fun ({C, T, TS, Ref}, D) ->
                        case dict:find(C, D) of
                            error ->
                                dict:store(C, [{T, TS, Ref}], D);
                            {ok, OD} ->
                                dict:store(C, [{T, TS, Ref} | OD],
                                           D)
                        end
                end,
                dict:new(),
                ets:tab2list(?MODULE)).
