%% @copyright Geoff Cant 2011
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Creates a record to proplist function for a given module (compiled with debug_info)
%% @end

f(MagicDecoder).
MagicDecoder = fun (Info, Record) ->
                       case proplists:get_value(element(1, Record), Info) of
                           undefined -> Record;
                           Fields when (length(Fields) + 1) =:= tuple_size(Record) ->
                               [{K, element(I, Record)} ||
                                   {K, I} <- lists:zip(Fields,
                                                       lists:seq(2,tuple_size(Record)))]
                       end
               end.
f(MagicDecoderRing).
MagicDecoderRing = fun (Module) ->
                           Info = [{Name, [element(3, element(3, F)) ||F<- Fields]}
                                   || {attribute, _, record, {Name, Fields}} <- element(2, proplists:get_value(abstract_code,element(2, element(2, beam_lib:chunks(code:which(Module), [abstract_code])))))],
                           fun (Record) -> MagicDecoder(Info, Record) end
                   end.

%% D = MagicDecoderRing(SomeModule), D(SomeModuleRecordTuple) -> proplist.
