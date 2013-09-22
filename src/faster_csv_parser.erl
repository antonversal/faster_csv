%% Copyright
-module(faster_csv_parser).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-behaviour(gen_server).

%% API
-export([start_link/1, parse/3, end_of_file/1]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

%% ====================================================================
%% API
%% ====================================================================
start_link(Reciever) ->
  gen_server:start_link(?MODULE, [Reciever], []).

parse(Pid, LineNumber, Line) ->
  gen_server:cast(Pid, {line, LineNumber, Line}).

end_of_file(Pid) ->
  gen_server:cast(Pid, eof).

%% ====================================================================
%% gen_server callbacks
%% ====================================================================

init([Reciever]) ->
  io:format("~p (~p) starting...~n", [?MODULE, self()]),
  process_flag(trap_exit, true),
  {ok, Reciever}.

handle_call(_Request, _From, State) ->
  {noreply, State}.

handle_cast(eof, State) ->
  io:format("~p (~p) done.~n", [?MODULE, self()]),
  State ! {parser_is_done, self()},
  {noreply, State};

handle_cast({line, LineNumber, Line}, State) ->
  %io:format("Parser got: ~w ~n", [{LineNumber, Line}]),
  List = parse_line(Line),
  %io:format("Send to process: ~w ~n", [{LineNumber, List}]),
  %State ! {LineNumber, List},
  {noreply, State};

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  io:format("~p (~p) Stopping...~n", [?MODULE, self()]),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================

parse_line(Line) ->
  %io:format("Got: ~p ~n", [Line]),
  %{Encoding, Length} = unicode:bom_to_encoding(Line),
  %io:format("Encoding: ~p, ~p ~n", [Encoding, Length]),
  parse_line(Line, $,, none).

parse_line(Line, Spliter, Delimiter) ->
  parse_line(Line, Spliter, Delimiter, [], [], ok).

parse_line(<<>>, _, _, _, _, started) ->
    throw(no_closed_text_delimiter);

parse_line(<<Delimiter, "\n" >>, _, Delimiter, Fields, Acc, started) when Delimiter /= none->
    lists:reverse(join_list(Fields, Acc));

parse_line(<<Delimiter>>, _, Delimiter, Fields, Acc, started) when Delimiter /= none->
    lists:reverse(join_list(Fields, Acc));

parse_line(<<"\n">>, _, _, Fields, Acc, ok) ->
    lists:reverse(join_list(Fields, Acc));

parse_line(<<>>, _, _, Fields, Acc, ok) ->
    lists:reverse(join_list(Fields, Acc));

parse_line(<<Delimiter, Rest/bitstring>>, Spliter, Delimiter, Fields, [], ok) when Delimiter /= none ->
  parse_line(Rest, Spliter, Delimiter, Fields, [], started);

parse_line(<<Delimiter, Spliter, Rest/bitstring>>, Spliter, Delimiter, Fields, Acc, started) when Delimiter /= none ->
  parse_line(Rest, Spliter, Delimiter, join_list(Fields, Acc), [], ok);

parse_line(<<Spliter, Delimiter, Rest/bitstring>>, Spliter, Delimiter, Fields, Acc, ok) when Delimiter /= none ->
  parse_line(Rest, Spliter, Delimiter, join_list(Fields, Acc), [], started);

parse_line(<<Spliter, Rest/bitstring>>, Spliter, Delimiter, Fields, Acc, ok) ->
  parse_line(Rest, Spliter, Delimiter, join_list(Fields, Acc), [], ok);

%solves embedded double-delimiter characters
parse_line(<<Delimiter, Delimiter, Rest/bitstring>>, Spliter, Delimiter, Fields, Acc, State) when Delimiter /= none ->
  parse_line(Rest, Spliter, Delimiter, Fields, [Delimiter|Acc], State);

parse_line(<<Char, Rest/bitstring>>, Spliter, Delimiter, Fields, Acc, State) ->
  parse_line(Rest, Spliter, Delimiter, Fields, [Char|Acc], State).

join_list(Fields, []) ->  
  [nil| Fields];

join_list(Fields, Acc) ->
  [lists:reverse(Acc)| Fields].

% parse_line(Line, Acc) ->
%   lists:reverse(parse_line(Line, [], Acc)).
% parse_line([End], Str, Acc) ->
%   Str0 = case End of
%            $\n ->
%              Str;
%            _ -> % NOTE: The last line of the file
%              [End|Str]
%          end,
%   case Str0 of
%     [] ->
%       Acc;
%     _ ->
%       [string:strip(lists:reverse(Str0))|Acc]
%   end;
% parse_line([$", $,|R], _, Acc) ->
%   parse_line(R, [], ["\""|Acc]);
% parse_line([$"|R], Str, Acc) ->
%   parse_string(R, Str, Acc);
% parse_line([$,|R], Str, Acc) ->
%   parse_line(R, [], [string:strip(lists:reverse(Str))|Acc]);
% parse_line([I|R], Str, Acc) ->
%   parse_line(R, [I|Str], Acc).

% parse_string([$", $,|R], Str, Acc) ->
%   parse_line(R, [], [string:strip(lists:reverse(Str))|Acc]);
% parse_string([$"|R], Str, Acc) ->
%   parse_line(R, [], [string:strip(lists:reverse(Str))|Acc]);
% parse_string([I|R], Str, Acc) ->
%   parse_string(R, [I|Str], Acc).


-ifdef(TEST).
parse_line_test_() ->
  [{"Parsing line with comma separator",
   with_comma()},
   {"Parsing line with pipe separator",
   with_pipe()},
   {"Parses line with text delimiter(\")",
   with_text_delimiter()},
   {"Parses line when text have delimiter(\")",
   with_one_text_delimiter()},
   {"Parses line when text delimiter set as none",
   with_text_delimiter_is_none()},
   {"Parses line when text delimiter set as pipe",
   with_pipe_delimiter()},
   {"Parses line when text is delimited and have comma inside",
   with_comma_inside_delimited_text()},
   {"Parses line when a at the end of the text", 
   one_text_delimiter_at_end()},
   {"Parses line when text have two delimiters", 
   with_two_text_delimiter_in_text()},
   {"When text started from delimiter and have not closed delimiter",
   one_text_delimiter_at_begining()},
   {"When all texts delimited", 
   when_all_lines_is_delimited()},
   {"When one of the text is null",
   when_value_is_null()},
   {"When one of the text is blank",
   when_value_is_blank()},
   {"When delimiter at the end of the line",
    [when_LF_at_the_end_with_delimiter(),
     when_LF_at_the_end_without_delimiter()]}
   ].

with_pipe() ->
  Line = <<"managerID|awardID|yearID|lgID|tie|notes">>,
  List = parse_line(Line, $|, none),
  ExpectedList = ["managerID","awardID","yearID","lgID","tie","notes"],
  ?_assertEqual(ExpectedList, List).

with_comma() ->
  Line = <<"managerID,awardID,yearID,lgID,tie,notes">>,
  List = parse_line(Line, $,, none),
  ExpectedList = ["managerID","awardID","yearID","lgID","tie","notes"],
  ?_assertEqual(ExpectedList, List).

with_text_delimiter() ->
  Line = <<"managerID,\"awardID some text\",yearID,lgID,tie,notes">>,
  List = parse_line(Line, $,, $"),
  ExpectedList = ["managerID","awardID some text","yearID","lgID","tie","notes"],
  ?_assertEqual(ExpectedList, List).

with_text_delimiter_is_none() ->
  Line = <<"managerID,\"awardID some text\",yearID,lgID,tie,notes">>,
  List = parse_line(Line, $,, none),
  ExpectedList = ["managerID","\"awardID some text\"","yearID","lgID","tie","notes"],
  ?_assertEqual(ExpectedList, List).  

with_pipe_delimiter() ->
  Line = <<"managerID,|awardID some text|,yearID,lgID,tie,notes">>,
  List = parse_line(Line, $,, $|),
  ExpectedList = ["managerID","awardID some text","yearID","lgID","tie","notes"],
  ?_assertEqual(ExpectedList, List).

with_one_text_delimiter() ->
  Line = <<"managerID,awardID\"some text,yearID,lgID,tie,notes">>,
  List = parse_line(Line, $,, $"),
  ExpectedList = ["managerID","awardID\"some text","yearID","lgID","tie","notes"],
  ?_assertEqual(ExpectedList, List).

with_two_text_delimiter_in_text() ->
  Line = <<"managerID,awardID\"\"some text,yearID,lgID,tie,notes">>,
  List = parse_line(Line, $,, $"),
  ExpectedList = ["managerID","awardID\"some text","yearID","lgID","tie","notes"],
  ?_assertEqual(ExpectedList, List).

with_comma_inside_delimited_text() ->
  Line = <<"managerID,\"awardID, some text\",yearID,lgID,tie,notes">>,
  List = parse_line(Line, $,, $"),
  ExpectedList = ["managerID","awardID, some text","yearID","lgID","tie","notes"],
  ?_assertEqual(ExpectedList, List).

one_text_delimiter_at_end() ->
  Line = <<"managerID,awardID\",yearID,lgID,tie,notes">>,
  List = parse_line(Line, $,, $"),
  ExpectedList = ["managerID","awardID\"","yearID","lgID","tie","notes"],
  ?_assertEqual(ExpectedList, List).  

one_text_delimiter_at_begining() ->
  Line = <<"managerID,\"awardIDsome text,yearID,lgID,tie,notes">>,
  ?_assertThrow(no_closed_text_delimiter, parse_line(Line, $,, $")).

when_all_lines_is_delimited() ->
  Line = <<"\"managerID\",\"awardID, some text\",\"yearID\",\"lgID\",tie,\"notes\"">>,
  List = parse_line(Line, $,, $"),
  ExpectedList = ["managerID","awardID, some text","yearID","lgID","tie","notes"],
  ?_assertEqual(ExpectedList, List). 

when_value_is_null() ->
  Line = <<"\"managerID\",\"awardID, some text\",,\"lgID\",tie,\"notes\"">>,
  List = parse_line(Line, $,, $"),
  ExpectedList = ["managerID","awardID, some text",nil,"lgID","tie","notes"],
  ?_assertEqual(ExpectedList, List). 

when_value_is_blank() ->
  Line = <<"\"managerID\",\"awardID, some text\",\"\",\"lgID\",tie,\"notes\",\"\"">>,
  List = parse_line(Line, $,, $"),
  ExpectedList = ["managerID","awardID, some text",nil,"lgID","tie","notes",nil],
  ?_assertEqual(ExpectedList, List).   

when_LF_at_the_end_with_delimiter() ->
  Line = <<"\"managerID\",\"awardID, some text\",\"\",\"lgID\",tie,\"notes\",\"\"\n">>,
  List = parse_line(Line, $,, $"),
  ExpectedList = ["managerID","awardID, some text",nil,"lgID","tie","notes",nil],
  ?_assertEqual(ExpectedList, List).   

when_LF_at_the_end_without_delimiter() ->
  Line = <<"\"managerID\",\"awardID, some text\",\"\",\"lgID\",tie,notes\n">>,
  List = parse_line(Line, $,, $"),
  ExpectedList = ["managerID","awardID, some text",nil,"lgID","tie","notes"],
  ?_assertEqual(ExpectedList, List).   

-endif.