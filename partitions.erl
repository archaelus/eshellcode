%% @copyright Geoff Cant 2011
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Erlang shell code to create and remove network parititions for testing.
%% Assumes you're not using per-node cookies, and that your cookie
%% isn't 'disallowed'.
%% @end

f(Part).
Part = fun (Node) ->
               erlang:set_cookie(Node, 'disallowed'),
               net_kernel:disconnect(Node)
       end.

f(UnPart).
UnPart = fun (Node) ->
                 erlang:set_cookie(Node, erlang:get_cookie())
         end.

f(Join).
Join = fun (Node) ->
               UnPart(Node),
               net_kernel:connect(Node)
       end.
