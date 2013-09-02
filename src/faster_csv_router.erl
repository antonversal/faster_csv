%% Copyright
-module(faster_csv_router).

-behaviour(gen_server).

%% API
-export([start_link/1, line/2, end_of_file/1]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-record(state, {parser_pids = [], id}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Id) ->
  Name = name(Id),
  gen_server:start_link({local, Name}, ?MODULE, [Id], []).

line(Id, Line) ->
  Name = name(Id),
  gen_server:cast(Name, {line, Line}).

end_of_file(Id) ->
  Name = name(Id),
  gen_server:cast(Name, eof).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================
init([Id]) ->
  io:format("~p (~p) starting...~n", [?MODULE, self()]),
  process_flag(trap_exit, true),
  Pids = faster_csv_parser_sup:children_pids(Id),
  {ok, #state{parser_pids = Pids, id = Id} }.

handle_call(_Request, _From, State) ->
  {reply, ingnored, State}.

handle_cast({line, Line}, State) ->
  %%io:format("State is: ~w ~n", [State]),
  Pids = send_to_parser(Line, State#state.parser_pids),
  %%io:format("NewState is: ~w ~n", [NewSate]),
  {noreply, State#state{parser_pids = Pids}};

handle_cast(eof, State) ->
  io:format("File is ended! ~n"),
  send_to_eof_parser(State#state.parser_pids),
  {noreply, State};

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  io:format("~p (~p) stopping...~n", [?MODULE, self()]),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================
send_to_parser(Line, Parsers) ->
  Pid = hd(Parsers),
  faster_csv_parser:parse(Pid, Line),
  tl(Parsers) ++ [Pid].

send_to_eof_parser([]) ->
  ok;

send_to_eof_parser(Pids) ->
  Pid = hd(Pids),
  faster_csv_parser:end_of_file(Pid),
  send_to_eof_parser(tl(Pids)).

name(Id) ->
  faster_csv_naming:name(?MODULE, Id).