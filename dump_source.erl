%% @copyright Geoff Cant 2009
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Reprints the source code of a module compiled with debug_info.
%% @end

PrintSrc = fun (Module) ->
                   {ok,{_,[{abstract_code,{_,AC}}]}} =
                       beam_lib:chunks(code:which(Module),[abstract_code]),
                   io:put_chars(erl_prettypr:format(erl_syntax:form_list(AC)))
           end.
