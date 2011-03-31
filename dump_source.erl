%% @copyright Geoff Cant 2009
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Reprints the source code of a module compiled with debug_info.
%% @end

f(GetAST).
GetAST = fun (Module) ->
                   {ok,{_,[{abstract_code,{_,AC}}]}} =
                       beam_lib:chunks(code:which(Module),[abstract_code]),
                  erl_syntax:form_list(AC)
          end.

f(GetSrc).
GetSrc = fun (Module) ->
                 erl_prettypr:format(GetAST(Module))
         end.

f(PrintSrc).
PrintSrc = fun (Module) ->
                   io:format("~s", [GetSrc(Module)])
           end.

