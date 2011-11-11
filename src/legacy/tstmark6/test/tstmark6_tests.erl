%%%-------------------------------------------------------------------
%%% @author David <dlapsley@lib1-lt.haystack.mit.edu>
%%% @copyright (C) 2011, MIT Haystack Observatory
%%% @doc
%%%
%%% @end
%%% Created : 8 Sep 2011 by David <dlapsley@lib1-lt.haystack.mit.edu>
%%%-------------------------------------------------------------------
-module(tstmark6_tests).
-author("dlapsley@haystack.mit.edu").
-include_lib("eunit/include/eunit.hrl").
-include("main.hrl").

-export([server/2]).

my_test_() ->
    [
     {timeout, ?TEST_TIMEOUT_MS, ?_test(basic())}
    ].

start_server(Responses) ->
    {ok, spawn(?MODULE, server, [?DEFAULT_PORT, Responses])}.

server(Listen_port, Responses) ->
    {ok, Listen_socket} = gen_tcp:listen(Listen_port, ?TCP_OPTIONS),
    wait_connect(Listen_socket, Responses).

wait_connect(Listen_socket, List) ->
    {ok, Socket} = gen_tcp:accept(Listen_socket),
    handle_request(Socket, List).

handle_request(Socket, [H|T]) ->
    case gen_tcp:recv(Socket, 0) of
	{ok, _Command} ->
	    error_logger:info_msg("R: ~p~n", [H]),
	    Xml ="quit",
	    ok = gen_tcp:send(Socket, Xml),
	    handle_request(Socket, T);
	{error, Reason} ->
	    error_logger:info_msg("R: done: ~p~n", [Reason])
    end;
handle_request(Socket, []) ->
    ok.

commands() ->
    [
     "input_stream=add:RDBE1:vdif:eth0:192.162.1.38;",
     "input_stream=add:RDBE2:vdif:eth0:192.162.1.40;",
     "record=on:076-1233:exp123:wf;",
     "vol_stack?;",
     "record=off",
     "scan_info?;",
     "scan_check?;"
    ].

% Start of tests...
basic() ->
    error_logger:info_msg("tstmark6_tests:basic!()~n"),
    process_flag(trap_exit, true),
    {ok, Dummy_pid} = start_server([]),
    timer:sleep(1000),
    {ok, Client_pid} = tstmark6:start_server(true),
    timer:sleep(1000),
    lists:foreach(fun(X) -> Client_pid ! {self(), X} end, commands()),
    timer:sleep(1000),
    exit(Client_pid, "Shutting down tstmark6 pid."),
    exit(Dummy_pid, "Sutting down dummy pid."),
    ok.

