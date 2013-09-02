%% Copyright
-module(faster_csv_reader).

-behaviour(gen_server).

%% API
-export([start_link/1, read/2]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% for timer
-export([read_file/2]).

%% ===================================================================
%% API functions
%% ===================================================================
start_link(Id) ->
  Name = name(Id),
  gen_server:start_link({local, Name}, ?MODULE, Id,[]).
read(Id, FileName) ->
  gen_server:cast(name(Id), {read, FileName}).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================
init(Id) ->
  io:format("~p (~p) starting...~n", [?MODULE, self()]),
  process_flag(trap_exit, true),
  {ok, Id}.

handle_call(_Request, _From, State) ->
  {reply, ingnored, State}.

handle_cast({read, FileName}, State) ->
  Ret = timer:tc(faster_csv_reader, read_file, [State, FileName]),
  io:format("Timer returns ~w~n", [Ret]),
  {noreply, State};

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Reason, _State) ->
  io:format("~p (~p) stopping...~n", [?MODULE, self()]),
  ok.

%% ====================================================================
%% Internal functions
%% ====================================================================
open_file(FileName) ->
  {ok, File} = file:open(FileName, [read, raw, read_ahead]),
  File.

close_file(File) ->
  ok = file:close(File).

read_lines(Id, File) ->
  case file:read_line(File) of
    {ok, Data} ->
        faster_csv_router:line(Id,Data),
        read_lines(Id, File);
    eof        ->
      faster_csv_router:end_of_file(Id)
  end.

read_file(Id, FileName) ->
  File = open_file(FileName),
  read_lines(Id, File),
  close_file(File).

name(Id) ->
  faster_csv_naming:name(?MODULE, Id).