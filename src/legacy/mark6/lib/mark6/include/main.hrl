%%%-------------------------------------------------------------------
%%% @author David <dlapsley@lib1-lt.haystack.mit.edu>
%%% @copyright (C) 2011, MIT Haystack Observatory
%%% @doc
%%%
%%% @end
%%% Created : 29 Mar 2011 by David <dlapsley@lib1-lt.haystack.mit.edu>
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%% Defines
%%%-------------------------------------------------------------------

%% M6 Server.
-define(DEFAULT_LISTEN_PORT, 14242).
-define(DEFAULT_BLOCK_SIZE, 1024).

%% Xcube connectivity.
-define(DEFAULT_HOST, "localhost").
-define(DEFAULT_PORT, 6050).
-define(DEFAULT_RCV_TO, 1000).
-define(DEFAULT_CONNECT_RETRIES, 5).

-define(TCP_OPTIONS,[list,{packet, 0},{active, false},{reuseaddr, true}]).

-define(XML_PROLOG, "").
-define(XML_PRETTY_PRINTING, true).

%% Enable/disable eunit tests.
-define(NOTEST, false).

%%%-------------------------------------------------------------------
%% Records
%%%-------------------------------------------------------------------

%% For xmerl processing of m6 responses.
-record(param, {name, type, value}).
-record(m6_resp, {retval, reason, name, params}).
-record(m6_input_stream, {stream_label, data_format, interface_id,
			  filter_address}).
