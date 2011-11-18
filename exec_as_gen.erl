%% @copyright Geoff Cant 2010
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @doc Erlang shell code to cause a gen* process to evaluate a fun.
%%
%% @license This code was translated from an ancient tome in the
%% Arkham university library (restricted section, 4th vault). As such,
%% use of this code is restricted to only those who see it as the
%% manifestly evil. Using this code under any circumstances other than in
%% dire need while debugging revokes your right to use, view, posess
%% said code.
%% @end

f(EvalAs).
EvalAs = fun (Pid, Fun) when is_function(Fun, 0) ->
                 Invocation = <<"Ph'nglui mglw'nafh "
                                "Cthulhu R'lyeh wgah'nagl fhtan">>,
                 TerrifyingResponse = <<"Ia! Ia! Cthulhu fhtagn">>,
                 Ref = make_ref(),
                 Caller = self(),
                 F = fun (nostate, {in, {hack,
                                         Invo,
                                         Ref}}, ProcState)
                           when Invo =:= Invocation ->
                             Caller ! {hack, TerrifyingResponse, Ref,
                                       catch Fun()},
                             done
                     end,
                 sys:install(Pid, {F,nostate}),
                 Pid ! {hack, Invocation, Ref},
                 receive
                     {hack, TerrifyingResponse, Ref, Result} ->
                         {TerrifyingResponse, Result}
                 after timer:seconds(5) ->
                         timeout
                 end
         end.

%% Eldrich Example -- Erich Zann's 'Secrets Best Forgotten (vol 1)'
EvalAs(application_controller,
       fun () -> io:format("Muahahaha, I am ~p!~n", [self()]) end).
