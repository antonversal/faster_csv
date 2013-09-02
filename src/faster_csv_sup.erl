-module(faster_csv_sup).

-behaviour(supervisor).

%% API
-export([start_link/3]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, V), {I, {I, start_link, V}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Id, ParserWrkCount, Reciever) ->
  Name = name(Id),
  supervisor:start_link({local, Name}, ?MODULE, [Id, ParserWrkCount, Reciever]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([Id, ParserWrkCount, Reciever]) ->
  process_flag(trap_exit, true),
  io:format("~p (~p) starting...~n", [?MODULE, self()]),
  Reader = ?CHILD(faster_csv_reader, worker, [Id]),
  ParserSup = ?CHILD(faster_csv_parser_sup, worker, [Id, ParserWrkCount, Reciever]),
  Router = ?CHILD(faster_csv_router, worker, [Id]),
  {ok, { {one_for_all, 5, 10}, [Reader, ParserSup, Router]}}.

name(Id) ->
  faster_csv_naming:name(?MODULE, Id).