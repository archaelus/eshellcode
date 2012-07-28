%% Copyright (c) 2010 Jacob Vorreuter <jacob.vorreuter@gmail.com>
%% Copyright (c) 2012 Geoff Cant <nem@erlang.geek.nz>
%%
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%%
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
-module(tcp_acceptor).
-behaviour(gen_server).

%% API
-export([start/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SOCK_OPTS, [binary,
                    {reuseaddr, true}, {packet, raw},
                    {keepalive, true}, {nodelay, true},
                    {backlog, 1000}, {active, false}]).

-define(CONN_MOD, tcp_reader).

-record(state, {listener, acceptor, accept = true, reader_args}).

%%====================================================================
%% API functions
%%====================================================================
start(Port, Args) ->
    gen_server:start(?MODULE, [Port, Args], []).

start_link(Port, Args) ->
    gen_server:start_link(?MODULE, [Port, Args], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([Port, Args]) ->
    process_flag(trap_exit, true),
    case gen_tcp:listen(Port, ?SOCK_OPTS) of
        {ok, LSock} ->
            {ok, Ref} = prim_inet:async_accept(LSock, -1),
            {ok, #state{listener=LSock, acceptor=Ref,
                        reader_args=Args}};
        Error ->
            {stop, Error}
    end.

handle_call(Request, _From, State) ->
    error_logger:warning_msg("~p unknown_call: ~p~n",
                             [{?MODULE, self()}, Request]),
    {noreply, State}.

handle_cast(Msg, State) ->
    error_logger:warning_msg("~p unknown_cast: ~p~n",
                             [{?MODULE, self()}, Msg]),
    {noreply, State}.

handle_info({inet_async, LSock, Ref, {ok, CSock}},
            State = #state{listener=LSock, acceptor=Ref}) ->
    try
        case set_sockopt(LSock, CSock) of
            ok -> ok;
            {error, Reason} -> exit({set_sockopt, Reason})
        end,

        {ok, Pid} = ?CONN_MOD:start(State#state.reader_args),
        gen_tcp:controlling_process(CSock, Pid),
        ?CONN_MOD:set_socket(Pid, CSock),

        %% Signal the network driver that we are ready to accept
        %% another connection
        case prim_inet:async_accept(LSock, -1) of
            {ok, NewRef} -> ok;
            {error, NewRef} -> exit({async_accept, inet:format_error(NewRef)})
        end,

        {noreply, State#state{acceptor=NewRef}}
    catch exit:Why ->
        error_logger:error_msg("Error in async accept: ~p.\n", [Why]),
        {stop, Why, State}
    end;

handle_info({inet_async, LSock, Ref, Error},
            #state{listener=LSock, acceptor=Ref}=State) ->
    error_logger:error_msg("Error in socket acceptor: ~p.\n", [Error]),
    {stop, Error, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State = #state{listener = LSock})
  when LSock =/= undefined ->
    catch gen_tcp:close(LSock),
    ok;
terminate(_Reason, #state{}) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Taken from prim_inet.  We are merely copying some socket options from the
%% listening socket to the new client socket.
set_sockopt(ListSock, CliSocket) ->
    true = inet_db:register_socket(CliSocket, inet_tcp),
    case prim_inet:getopts(ListSock,
                           [nodelay, keepalive, delay_send, priority, tos]) of
        {ok, Opts} ->
            case prim_inet:setopts(CliSocket, Opts) of
                ok    -> ok;
                Error -> catch gen_tcp:close(CliSocket), Error
            end;
        Error ->
            catch gen_tcp:close(CliSocket), Error
    end.
