%% Copyright
-module(faster_csv_parser_sup).

-behaviour(supervisor).
%% API
-export([start_link/5, add_child/1, children/1, children_pids/1]).

%% supervisor
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, V), {I, {I, start_link, V}, temporary, 1000, Type, [I]}).

%% ====================================================================
%% API
%% ====================================================================
start_link(Id, ParserWrkCount, Receiver, Splitter, Delimiter) ->
  Name = name(Id),
  io:format("Count: ~w ~n", [ParserWrkCount]),
  Res = supervisor:start_link({local, Name}, ?MODULE, [Receiver, Splitter, Delimiter]),
  create_parsers(Id, ParserWrkCount),
  Res.

add_child(Id) ->
  supervisor:start_child(name(Id), []).

children(Id) ->
  supervisor:which_children(name(Id)).

children_pids(Id) ->
  lists:map(fun(X) -> element(2,X) end,children(Id)).

%% ===================================================================
%% supervisor callbacks
%% ===================================================================
init([Receiver, Splitter, Delimiter]) ->
  process_flag(trap_exit, true),
  io:format("~p (~p) starting...~n", [?MODULE, self()]),
  Child = ?CHILD(faster_csv_parser, worker, [Receiver, Splitter, Delimiter]),
  {ok, {{simple_one_for_one, 5, 30},[Child]}}.

%% ====================================================================
%% Internal functions
%% ====================================================================
create_parsers(Id, 1) ->
  add_child(Id),
  ok;

create_parsers(Id, Count) ->
  add_child(Id),
  create_parsers(Id,Count - 1).

name(Id) ->
  faster_csv_naming:name(?MODULE, Id).