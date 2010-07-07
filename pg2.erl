%% @copyright Geoff Cant 2010
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Erlang shell code to fix pg2 tables.
%% @end

f(Pg2Groups).
Pg2Groups = fun () ->
                    ets:select(pg2_table,
                               ets:fun2ms(fun ({{group, G}}) -> G end))
            end.
f(Pg2GroupMembers).
Pg2GroupMembers = fun (Groups) ->
                          [{G, lists:usort(ets:select(pg2_table,
                                                      ets:fun2ms(fun ({{pid, Pid, Gr}})
                                                                       when G =:= Gr ->
                                                                         Pid
                                                                 end)))}
                           || G <- Groups]
                  end.
f(Pg2Pids).
Pg2Pids = fun () ->
                  lists:usort(ets:select(pg2_table,
                                         ets:fun2ms(fun ({{ref, R}, Pid}) when is_reference(R) -> Pid end)))
          end.
f(Pg2PidGrps).
Pg2PidGrps = fun (Pids) ->
                     [{P, lists:usort(ets:select(pg2_table,
                                                 ets:fun2ms(fun ({{pid, P, Grp}}) -> Grp end)))}
                      || P <- Pids]
             end.
f(Pg2Refs).
Pg2Refs = fun () ->
                  lists:usort(ets:select(pg2_table,
                                         ets:fun2ms(fun ({{ref, R}, Pid}) when is_reference(R) ->
                                                            {Pid, R}
                                                    end)))
          end.
f(Correct).
Correct = fun () ->
                  Groups = Pg2Groups(),
                  GrpMembers = Pg2GroupMembers(Groups),
                  Pids = Pg2Pids(),
                  PidGrps = Pg2PidGrps(Pids),
                  Refs = Pg2Refs(),

                  lists:flatten([ [{{group, G}} || G <- Groups],
                                  [{{ref, Ref}, Pid} || {Pid, Ref} <- Refs],
                                  [{{ref, Pid}, Pid, Ref, length(proplists:get_value(Pid, PidGrps))}
                                   || {Pid, Ref} <- Refs],
                                  [ [{{local_member, Grp, Pid}}
                                     || Grp <- Grps]
                                    || {Pid, Grps} <- PidGrps, node() =:= node(Pid)],
                                  [ [[{{member, Grp, Pid}, 1},
                                      {{pid, Pid, Grp}}]
                                     || Grp <- Grps]
                                    || {Pid, Grps} <- PidGrps]
                                ])
          end.

f(Diff).
Diff = fun () ->
               Pg2List = ets:tab2list(pg2_table),
               Pg2 = sets:from_list(Pg2List),
               RightList = Correct(),
               Right = sets:from_list(RightList),
               [ case lists:keysearch(element(1, I), 1, Pg2List) of
                     {value, Wrong} ->
                         Pos = tuple_size(I),
                         {fixup, element(1,I), {Pos, element(Pos, I)}, element(Pos, Wrong)};
                     false ->
                         {insert, I}
                 end
                 || I <- sets:to_list(sets:subtract(Right, Pg2))
               ]
       end.


f(Diff2).
Diff2 = fun () ->
                NewData = Correct(),
                Right = sets:from_list(NewData),
                Wrong = sets:from_list(ets:tab2list(pg2_table)),
                sets:to_list(sets:subtract(Wrong, Right))
        end.

f(Fixup).
Fixup = fun () ->
                NewData = Correct(),
                Right = sets:from_list(NewData),
                Wrong = sets:from_list(ets:tab2list(pg2_table)),
                Deletable = sets:to_list(sets:subtract(Wrong, Right)),
                ets:insert(pg2_table, NewData)
        end.

f(ListDiff).
ListDiff = fun (L1, L2) ->
                   L1S = sets:from_list(L1),
                   L2S = sets:from_list(L2),
                   {sets:to_list(sets:subtract(L2, L1)),
                    sets:to_list(sets:subtract(L1, L2))}
           end.
