%% Copyright
-module(faster_csv_parser).

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
  List = parse_line(Line, []),
  State ! {LineNumber, List},
  {noreply, State};

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  io:format("~p (~p) Stoping...~n", [?MODULE, self()]),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================

parse_line(Line, Acc) ->
  lists:reverse(parse_line(Line, [], Acc)).
parse_line([End], Str, Acc) ->
  Str0 = case End of
           $\n ->
             Str;
           _ -> % NOTE: The last line of the file
             [End|Str]
         end,
  case Str0 of
    [] ->
      Acc;
    _ ->
      [string:strip(lists:reverse(Str0))|Acc]
  end;
parse_line([$", $,|R], _, Acc) ->
  parse_line(R, [], ["\""|Acc]);
parse_line([$"|R], Str, Acc) ->
  parse_string(R, Str, Acc);
parse_line([$,|R], Str, Acc) ->
  parse_line(R, [], [string:strip(lists:reverse(Str))|Acc]);
parse_line([I|R], Str, Acc) ->
  parse_line(R, [I|Str], Acc).

parse_string([$", $,|R], Str, Acc) ->
  parse_line(R, [], [string:strip(lists:reverse(Str))|Acc]);
parse_string([$"|R], Str, Acc) ->
  parse_line(R, [], [string:strip(lists:reverse(Str))|Acc]);
parse_string([I|R], Str, Acc) ->
  parse_string(R, [I|Str], Acc).