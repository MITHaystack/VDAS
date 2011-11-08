%%%-------------------------------------------------------------------
%%% @author David <dlapsley@lib1-lt.haystack.mit.edu>
%%% @copyright (C) 2011, David
%%% @doc
%%% Manages incoming VSIS requests. Implements a parallel TCP server.
%%% @end
%%% Created : 29 Mar 2011 by David <dlapsley@lib1-lt.haystack.mit.edu>
%%%-------------------------------------------------------------------
-module(m6_server).
-author("dlapsley@haystack.mit.edu").
-include("main.hrl").
-export([start_link/0, wait_connect/1, init/1, handle_request/2]).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    Listen_port = case application:get_env(listen_port) of
		      {ok, Port} -> Port;
		      undefined -> ?DEFAULT_LISTEN_PORT
		  end,
    error_logger:info_msg("listen_port ~w~n", [Listen_port]),
    {ok, spawn_link(?MODULE, init, [Listen_port])}.

%%--------------------------------------------------------------------
%% @doc
%% Initializes module.
%%
%% @spec init(ListenPort) -> ignore
%% @end
%%--------------------------------------------------------------------
init(Listen_port) ->
    {ok, Listen_socket} = gen_tcp:listen(Listen_port, ?TCP_OPTIONS),
    wait_connect(Listen_socket).

%%--------------------------------------------------------------------
%% @doc
%% Main server processing loop.
%%
%% @spec wait_connect(Listen_port) -> {ok, Pid} | ignore
%%                                    | {error, Error}
%% @end
%%--------------------------------------------------------------------
wait_connect(Listen_socket) ->
    {ok, Socket} = gen_tcp:accept(Listen_socket),
    Pid = spawn_link(?MODULE, handle_request, [Socket, []]),
    gen_tcp:controlling_process(Socket, Pid),
    wait_connect(Listen_socket).

%%--------------------------------------------------------------------
%% @doc
%% Handle incoming request in a seperate micro-process.
%%
%% @spec wait_connect(Socket, List) -> {ok, Pid} | ignore
%%                                     | {error, Error}
%% @end
%%--------------------------------------------------------------------
handle_request(Socket, List) ->
    case gen_tcp:recv(Socket, 1) of
	{ok, ";"} ->
	    Command = lists:reverse(List) ++ ";",
	    error_logger:info_msg("m6_server:handle_request request ~p~n", [Command]),
	    {ok, Reply} = m6_vsis:handle(Command),
	    error_logger:info_msg("m6_server:handle_request response ~p~n", [Reply]),
	    gen_tcp:send(Socket, Reply),
	    handle_request(Socket, []);
	{ok, Char} ->
	    handle_request(Socket, [Char|List]);
	Error ->
	    error_logger:info_msg("Recv done: ~p~n", [Error])
    end.

