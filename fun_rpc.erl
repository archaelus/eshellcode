%% @copyright Geoff Cant 2009, 2010
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Erlang shell code to use funs with the rpc module.
%% @end

f(Multicall).
Multicall = fun (Fun) when is_function(Fun,0) ->
                     element(1, rpc:multicall(erlang, apply, [ Fun, [] ]))
             end.

f(Ncall).
Ncall = fun (Nodes, Fun) when is_function(Fun,0) ->
                     element(1, rpc:multicall(Nodes, erlang, apply, [ Fun, [] ]))
        end.

f(NodesCalled).
NodesCalled = fun (Name) ->
                      [N || N <- nodes(known),
                            lists:member(Name, string:tokens(atom_to_list(N), "@"))]
              end.

f(Call).
Call = fun (Node, Fun) when is_function(Fun,0) ->
                     rpc:call(Node, erlang, apply, [ Fun, [] ])
       end.

f(NamedCall).
NamedCall = fun (Name, Fun) ->
                    Ncall(NodesCalled(Name), Fun)
            end.

f(ListDiff).
ListDiff = fun (L1, L2) ->
                   L1S = sets:from_list(L1),
                   L2S = sets:from_list(L2),
                   {sets:to_list(sets:subtract(L2S, L1S)),
                    sets:to_list(sets:subtract(L1S, L2S))}
           end.

f(EvalDiff).
EvalDiff = fun (Node1, Node2, F) ->
                   ListDiff(Call(Node1, F),
                            Call(Node2, F))
           end.
