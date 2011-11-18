%% @copyright Geoff Cant 2009
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Alter table copy types to match types on a specific node.
%% @end

f(SchemaDiff).
SchemaDiff = fun (IdealNode, OtherNodes) ->
                     Tables = rpc:call(IdealNode, mnesia, system_info, [tables]),
                     TableTypes = [{T, rpc:call(IdealNode, mnesia, table_info, [T, storage_type])}
                                   || T <- Tables],
                     [{Table, Type, [ case rpc:call(N, mnesia, table_info, [Table, storage_type]) of
                                          Type -> {ok, N};
                                          unknown ->
                                              {missing, N};
                                          Other ->
                                              {different_type, Other, N}
                                      end
                                      || N <- OtherNodes]}
                      || {Table, Type} <- TableTypes]
             end.

f(Rebalance).
Rebalance = fun (IdealNode, OtherNodes) ->
                    Tables = rpc:call(IdealNode, mnesia, system_info, [tables]),
                    TableTypes = [{T, rpc:call(IdealNode, mnesia, table_info, [T, storage_type])}
                                  || T <- Tables],
                    [{Table, Type, [ case rpc:call(N, mnesia, table_info, [Table, storage_type]) of
                                         Type -> N;
                                         unknown ->
                                             {atomic, ok} = mnesia:add_table_copy(Table, N, Type),
                                             {added, N};
                                         Other ->
                                             {atomic, ok} = mnesia:change_table_copy_type(Table, N, Type),
                                             {fixed, N}
                                     end
                                     || N <- OtherNodes]}
                     || {Table, Type} <- TableTypes]
            end.

