%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Snippet to route addresses based on directly attached networks.
%% @end
-module(interfaces).

-export([route/1
         ,route/2
         ,routes/0
        ]).

route(Targ) ->
    route(Targ, routes()).

route(Targ, Routes) ->
    sort_routes(routes_for(Targ, Routes)).

routes_for(Targ, Routes) ->
    [ RT || RT = {_IF, {Addr, Mask}} <- Routes,
            tuple_size(Targ) =:= tuple_size(Addr),
            match_route(Targ, Addr, Mask)
    ].

sort_routes(Routes) ->
    lists:sort(fun ({_, {_AddrA, MaskA}}, {_, {_AddrB, MaskB}}) ->
                       MaskA > MaskB
               end,
               Routes).

match_route(Targ, Addr, Mask)
  when tuple_size(Targ) =:= tuple_size(Addr),
       tuple_size(Targ) =:= tuple_size(Mask) ->
    lists:all(fun (A) -> A end,
              [element(I, Targ) band element(I, Mask)
               =:= element(I, Addr) band element(I, Mask)
               || I <- lists:seq(1, tuple_size(Targ)) ]).

routes() ->
    {ok, IFData} = inet:getifaddrs(),
    lists:append([ routes(IF, IFOpts) || {IF, IFOpts} <- IFData ]).

routes(IF, Opts) ->
    {_,Routes} = lists:foldl(fun parse_opts/2, {undefined, []}, Opts),
    [{IF, Route}
     || Route <- Routes].

parse_opts({addr, Addr}, {undefined, Routes}) ->
    {{addr, Addr}, Routes};
parse_opts({netmask, Mask}, {{addr, Addr}, Routes})
  when tuple_size(Mask) =:= tuple_size(Addr) ->
    {undefined, [{Addr, Mask} | Routes]};
parse_opts(_, Acc) -> Acc.
