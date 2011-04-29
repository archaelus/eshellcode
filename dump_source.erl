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

f(NPrintSrc).
NPrintSrc = fun (Module, Node) ->
                    File = rpc:call(Node, code,which,[Module]),
                    {ok,{_,[{abstract_code,{_,AC}}]}} =
                        rpc:call(Node, beam_lib,chunks, [File,[abstract_code]]),
                    io:put_chars(erl_prettypr:format(erl_syntax:form_list(AC)))
            end.
