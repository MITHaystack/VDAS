%%%-------------------------------------------------------------------
%%% @author David <dlapsley@lib1-lt.haystack.mit.edu>
%%% @copyright (C) 2011, MIT Haystack Observatory
%%% @doc
%%%
%%% @end
%%% Created : 29 Mar 2011 by David <dlapsley@lib1-lt.haystack.mit.edu>
%%%-------------------------------------------------------------------
-module(mark6_sup).
-behavior(supervisor).
-export([start_link/0]).
-export([init/1]).
-author("dlapsley@haystack.mit.edu").

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).
 
init(_Args) ->
    error_logger:info_msg("Init-ing supervisor.~n"),
    M6_server = ?CHILD(m6_server, worker),
    M6_state = ?CHILD(m6_state, worker),
    {ok, {{one_for_one, 5, 10}, [M6_server, M6_state] }}.
