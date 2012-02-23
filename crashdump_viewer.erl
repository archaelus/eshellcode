%% @copyright Geoff Cant 2011
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Crashdump viewer console file loading function.
%%
%% Instead of uploading the dump in a browser, copy it to the local
%% filesystem, then run LoadDump("/path/to/dump").
%% @end

%% Hipe compile some stuff
hipe:c(gb_trees, [load]).
hipe:c(crashdump_viewer, [load]).

webtool:start().
crashdump_viewer:start().

f(LoadDump).
LoadDump = fun (FileName) when is_list(FileName) ->
                   gen_server:call(crashdump_viewer_server,
                                   {read_file, "path=" ++ FileName})
           end.
