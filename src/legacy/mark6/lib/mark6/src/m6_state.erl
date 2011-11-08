%%%-------------------------------------------------------------------
%%% @author David <dlapsley@tigress.haystack.mit.edu>
%%% @copyright (C) 2011, David
%%% @doc
%%%
%%% @end
%%% Created : 19 Sep 2011 by David <dlapsley@tigress.haystack.mit.edu>
%%%-------------------------------------------------------------------
-module(m6_state).

-behaviour(gen_server).

%% API
-export([start_link/0, add_input_stream/2, dismount_input_stream/1,
	 get_input_streams/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {input_stream_table}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

add_input_stream(Label, Stream) ->
    gen_server:call(?MODULE, {add_input_stream, Label, Stream}).

dismount_input_stream(Label) ->
    gen_server:call(?MODULE, {dismount_input_stream, Label}).

get_input_streams() ->
    gen_server:call(?MODULE, {get_input_streams}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    error_logger:info_msg("Initializing m6_state."),
    Input_stream_table_id = ets:new(input_stream_table, [set]),
    {ok, #state{input_stream_table=Input_stream_table_id}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({add_input_stream, Label, Stream}, _From, State) ->
    Reply = ok,
    error_logger:info_msg("Inserting input_stream: ~p~n", [Stream]),
    ets:insert(State#state.input_stream_table, {Label, Stream}),
    {reply, Reply, State};
handle_call({dismount_input_stream, Label}, _From, State) ->
    Reply = ok,
    error_logger:info_msg("Removing input_stream: ~p~n", [Label]),
    ets:delete(State#state.input_stream_table, Label),
    {reply, Reply, State};
handle_call({get_input_streams}, _From, State) ->
    error_logger:info_msg("IST: ~p~n", [ ets:match_object(State#state.input_stream_table, '$1') ]),
    Streams = lists:map(fun(X) ->
				{Label, Object} = X,
				Object
			end, ets:match_object(State#state.input_stream_table, '$1')),
    Reply = {ok, Streams},
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
