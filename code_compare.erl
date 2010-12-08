%% @copyright Geoff Cant 2010
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Compare loaded with disk code.
%% @end

f(CmpModule).
CmpModule = fun (Module) ->
                    Secs = fun ({YY,MM,DD,H,M,S}) ->
                                   calendar:datetime_to_gregorian_seconds({{YY,MM,DD},{H,M,S}})
                           end,
                    {file, File} = code:is_loaded(Module),
                    {ok, {Module, [{compile_info, CI}]}} = beam_lib:chunks(File, [compile_info]),
                    LoadedTime = proplists:get_value(time, Module:module_info(compile)),
                    DiskTime = proplists:get_value(time, CI),
                    case LoadedTime of
                        DiskTime -> in_sync;
                        _ ->
                            [{loaded, LoadedTime},
                             {disk, DiskTime},
                             {disk_is_newer,
                              Secs(DiskTime) -
                                  Secs(LoadedTime)
                             }]
                    end
            end.

f(CmpApp).
CmpApp = fun (App) ->
                 [{M, CmpModule(M)}
                  || M  <- element(2, application:get_key(App, modules))]
         end.
