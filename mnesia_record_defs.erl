%% @copyright Geoff Cant 2010
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Use mnesia schema info to define records in the shell.
%% @end

io:format("~s~n", [string:join([io_lib:format("rd(~p, ~p).", [T, list_to_tuple(mnesia:table_info(T, attributes))]) || T <- mnesia:system_info(tables), T =/= schema], "\n")]).

% Now copy the result and paste it back into the shell as a command.
