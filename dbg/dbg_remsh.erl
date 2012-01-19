%% @copyright Geoff Cant 2012
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @doc Simplest dbg tracer process.
%% @end

dbg:tracer(process, {fun dbg:dhandler/2, group_leader()}).
