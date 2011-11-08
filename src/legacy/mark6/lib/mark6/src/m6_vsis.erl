%%-------------------------------------------------------------------
%%% @author David <dlapsley@lib1-lt.haystack.mit.edu>
%%% @copyright (C) 2011, MIT Haystack Observatory
%%% @doc
%%% Validates and parses VSI-S commands. Converts them to appropriate
%%% sequence of X3C commands.
%%% @end
%%% Created : 29 Mar 2011 by David <dlapsley@lib1-lt.haystack.mit.edu>
%%%-------------------------------------------------------------------
-module(m6_vsis).
-compile(export_all).
-author("dlapsley@haystack.mit.edu").
-include("main.hrl").

%%--------------------------------------------------------------------
%% Function: cmd_re_string() -> term()
%% Description: Provides access to vsis command regexp.
%%--------------------------------------------------------------------
cmd_re_string() -> "^(\\w+)=((\\w+)(:\\w+)*);\$".

%%--------------------------------------------------------------------
%% Function: qry_re_string() -> term()
%% Description: Provides access to vsis query regexp.
%%--------------------------------------------------------------------
qry_re_string()-> "^(\\w+)\\?((\\w+)(:\\w+)*)*;\$".

%%--------------------------------------------------------------------
%% Function: is_valid(X, Regexp) -> false | true
%% Description: Return whether or not X conforms to Regexp.
%%--------------------------------------------------------------------
is_valid(X, Regexp) ->	
    match == re:run(X, Regexp, [{capture, none}]).

%%--------------------------------------------------------------------
%% Function: parser(X, Regexp) -> {ok, {String, Cmd, Args} } |
%%                                {error, Reason }
%% Description: Parse string X using Regexp.
%%--------------------------------------------------------------------
parse(X, Regexp) ->
    case re:run(X, Regexp, [{capture, all, list}]) of
	{match, ParsedList} ->
	    [String,Cmd|Args] = ParsedList,
	    SplitArgs = split_args(Args),
	    %error_log:info_msg("String ~p Cmd ~p Args ~p~n", [String, Cmd, SplitArgs]),
	    {ok, {String, Cmd, SplitArgs}};
	_ ->
	    {error, "Parsing error"}
    end.

%%--------------------------------------------------------------------
%% Function:  split_args([H|T]) -> list() | []
%% Description: Split Arg string into list of individual args.
%%--------------------------------------------------------------------
split_args([H|_]) ->
    re:split(H, ":", [{return, list}]);
split_args([]) ->
    [].

%%--------------------------------------------------------------------
%% Function:  handleCommand) -> {ok, Reply} | {error, Error }
%% Description: Handle received commands.
%%--------------------------------------------------------------------
handle(Command) ->
    error_logger:info_msg("m6_vsis:handle -> ~p~n", [Command]),
    Cmd_re = cmd_re_string(),
    Qry_re = qry_re_string(),
    Reply = case is_valid(Command, Cmd_re) of
		true ->
		    case parse(Command, Cmd_re) of
			{ok, {_String, Cmd, Args}} ->
			    handle_command(list_to_atom(Cmd), Args);
			{error, Why} ->
			    {error, Why}
		    end;
		_ ->
		    case is_valid(Command, Qry_re) of
			true ->
			    case parse(Command, Qry_re) of
				{ok, {_String, Qry, Args}} ->
				    handle_query(list_to_atom(Qry), Args);
				{error, Why} ->
				    {error, Why}
			    end;
			Error ->
			    { error, "Invalid command string", Error }
		    end
	    end,
    {ok, Reply}.

%%--------------------------------------------------------------------
%% Function: handle_command(Type, Args) -> {ok,Result} | {error,Error}
%% Description: Handles a VSIS command. Maps to x3c and sends to 
%%              xcube system. Retrieves response and then returns it.
%%--------------------------------------------------------------------
handle_command(disk_state, Args) ->
    "received your disk_state command;";
    % x3c_server:send_x3c_cmd(set_volume_state, Args);
handle_command(disk_state_mask, Args) ->
    "received your disk_state_mask command;";
    % x3c_server:send_x3c_cmd(set_volume_state_mask, Args);
handle_command(input_stream, Args) ->
    case lists:nth(1, Args) of
	"add" ->
	    m6_state:add_input_stream(
	      lists:nth(2, Args),
	      #m6_input_stream{stream_label=lists:nth(2, Args),
			       data_format=lists:nth(3, Args),
			       interface_id=lists:nth(4, Args),
			       filter_address=lists:nth(5, Args)
			      }),
	    "!input_stream=0:0;";
	"dismount" ->
	    m6_state:dismount_input_stream(lists:nth(2, Args)),
	    "!input_stream=0:0;"
    end;
handle_command(msn, Args) ->
    "received your input_stream command;";
    % x3c_server:send_x3c_cmd(set_module_serial_number, Args);
handle_command(record, Args) ->
    "received your record command;";
    % x3c_server:send_x3c_cmd(set_record, Args);
handle_command(vol_def, Args) ->
    "received your vol_def command;";
    % x3c_server:send_x3c_cmd(define_volume, Args);
handle_command(vol_action, Args) ->
    "received your vol_action command;".
    % x3c_server:send_x3c_cmd(set_volume_action, Args).

%%--------------------------------------------------------------------
%% Function: handle_query(Type, Args) -> {ok,Result} | {error,Error}
%% Description: Handles a VSIS query. Maps to x3c and sends to 
%%              xcube system. Retrieves response and then returns it.
%%--------------------------------------------------------------------
handle_query(disk_state, Args) ->
    x3c_server:send_x3c_cmd(get_volume_state, Args);
handle_query(disk_state_mask, Args) ->
    x3c_server:send_x3c_cmd(get_volume_state_mask, Args);
handle_query(input_stream, Args) ->
    {ok, Streams} = m6_state:get_input_streams(),
    Stream_output = lists:map(fun(X) ->
				      [
				       X#m6_input_stream.stream_label,
				       X#m6_input_stream.data_format,
				       X#m6_input_stream.interface_id,
				       X#m6_input_stream.filter_address
				      ]
			      end, Streams),
    Stream_strings = lists:map(fun(X) ->
				       string:join(X, ":")
			       end, Stream_output),
    Stream_reply = string:join(Stream_strings, "\n"),
    error_logger:info_msg("Q4: ~p~n", [ Stream_reply ]),
    string:join([
		 "!input_stream?0:0:\n",
		 Stream_reply,
		 ";"
		], "");

handle_query(msn, Args) ->
    x3c_server:send_x3c_cmd(get_module_serial_number, Args);
handle_query(record, _Args) ->
    %% TODO: correct in document -> get_record_status -> get_record_info.
    x3c_server:send_x3c_cmd(get_record_status, []);
handle_query(vol_def, Args) ->
    x3c_server:send_x3c_cmd(get_volume_info, Args);
handle_query(data_check, Args) ->
    x3c_server:send_x3c_cmd(check_data, Args);
handle_query(disk_model, Args) ->
    x3c_server:send_x3c_cmd(get_disk_data, Args);
handle_query(disk_serial, Args) ->
    x3c_server:send_x3c_cmd(get_disk_data, Args);
handle_query(disk_size, Args) ->
    x3c_server:send_x3c_cmd(get_disk_data, Args);
handle_query(disk_perf, Args) ->
    x3c_server:send_x3c_cmd(get_disk_performance, Args);
handle_query(dts_id, _Args) ->
    x3c_server:send_x3c_cmd(get_recorder_info, []);
handle_query(status, _Args) ->
    x3c_server:send_x3c_cmd(get_system_status, []);
handle_query(rtime, Args) ->
    x3c_server:send_x3c_cmd(get_volume_usage, Args);
handle_query(vol_usage, Args) ->
    x3c_server:send_x3c_cmd(get_volume_usage, Args);
handle_query(vol_stack, Args) ->
    x3c_server:send_x3c_cmd(get_volume_usage, Args).

%%--------------------------------------------------------------------
%% Function: commands() -> list()
%% Description: Provides a list of all handled commands.
%%              Must be updated whenver new handlers are added.
%%--------------------------------------------------------------------
commands() ->
    [
     disk_state,
     disk_state_mask,
     input_stream,
     msn,
     record,
     vol_def,
     vol_action
    ].

%%--------------------------------------------------------------------
%% Function: commands() -> list()
%% Description: Provides a list of all handled commands.
%%              Must be updated whenver new handlers are added.
%%--------------------------------------------------------------------
queries() ->
    [
     disk_state,
     disk_state_mask,
     input_stream,
     msn,
     record,
     vol_def,
     data_check,
     disk_model,
     disk_serial,
     disk_size,
     disk_perf,
     dts_id,
     status,
     rtime,
     vol_usage
     ].
     
