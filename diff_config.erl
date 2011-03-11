%% @copyright Geoff Cant 2010
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Diff running OTP app config with startup
%% @end

f(GetConfig).
GetConfig = fun (App,SystemConfigFile,AppFile) ->
                    {ok, RunningConfig} = application:get_all_key(App),
                    {ok, [{application, App, StartupConfig}]} = file:consult(AppFile),
                    {ok, [SystemConfig]} = file:consult(SystemConfigFile ++ ".config"),
                    SystemAppConfig = proplists:get_value(App, SystemConfig),
                    MergedStartupEnv = {env, lists:foldl(fun ({K, V}, Acc) ->
                                                                 case lists:keymember(K, 1, Acc) of
                                                                     true ->
                                                                         lists:keyreplace(K, 1, Acc, {K,V});
                                                                     false ->
                                                                         [{K,V} | Acc]
                                                                 end
                                                         end,
                                                         proplists:get_value(env,StartupConfig),
                                                         SystemAppConfig)},
                    MergedStartupConfig = lists:keyreplace(env, 1, StartupConfig, MergedStartupEnv),
                    {RunningConfig, MergedStartupConfig}
            end.

f(DefaultConfig).
DefaultConfig = fun (App) ->
                        AppFile = filename:join([code:lib_dir(App), "ebin", atom_to_list(App) ++ ".app"]),
                        {ok, [[SystemConfigFile]]} = init:get_argument(config),
                        GetConfig(App, SystemConfigFile, AppFile)
                end.

f(DiffConfig2).
DiffConfig2 = fun ({RunningConfig,StartupConfig}) ->
                     RunningEnv = proplists:get_value(env, RunningConfig),
                     StartupEnv = proplists:get_value(env, StartupConfig),
                     RunningEnvKeys = proplists:get_keys(RunningEnv),
                     StartupEnvKeys = proplists:get_keys(StartupEnv),
                     SameAppK = [{K, proplists:get_value(K, RunningConfig)}
                                 || K <- proplists:get_keys(RunningConfig),
                                    proplists:get_value(K, RunningConfig) =:= proplists:get_value(K, StartupConfig)],
                     DiffAppK = [{K, proplists:get_value(K, RunningConfig), proplists:get_value(K, StartupConfig)}
                                 || K <- proplists:get_keys(RunningConfig),
                                    K =/= env,
                                    proplists:get_value(K, RunningConfig) =/= proplists:get_value(K, StartupConfig)],
                     SameEnv = [{K, proplists:get_value(K, RunningEnv)}
                                || K <- proplists:get_keys(RunningEnv),
                                   proplists:get_value(K, RunningEnv) =:= proplists:get_value(K, StartupEnv)],
                     DiffEnv = [{K, proplists:get_value(K, RunningEnv), proplists:get_value(K, StartupEnv)}
                                || K <- proplists:get_keys(RunningEnv),
                                   proplists:get_value(K, RunningEnv) =/= undefined,
                                   proplists:get_value(K, StartupEnv) =/= undefined,
                                   proplists:get_value(K, RunningEnv) =/= proplists:get_value(K, StartupEnv)],
                     Added = [ {K, proplists:get_value(K, RunningEnv)}
                               || K <- proplists:get_keys(RunningEnv) -- proplists:get_keys(StartupEnv)],
                     Deleted = [ {K, proplists:get_value(K, StartupEnv)}
                                 || K <- proplists:get_keys(StartupEnv) -- proplists:get_keys(RunningEnv)],
                               
                     [{app,[{same, SameAppK},{changed, DiffAppK}]},
                      {env,[{unchanged,SameEnv},
                            {changed,DiffEnv},
                            {added,Added},
                            {deleted,Deleted}]}]
             end.

f(DiffConfig).
DiffConfig = fun (App) ->
                     {RunningConfig, StartupConfig} = DefaultConfig(App),
                     DiffConfig2({RunningConfig,StartupConfig})
             end.

