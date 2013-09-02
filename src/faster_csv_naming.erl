-module(faster_csv_naming).

%% API
-export([name/2]).

%% ===================================================================
%% API functions
%% ===================================================================

name(Module, Id) ->
  list_to_atom(atom_to_list(Module) ++ "_"++ integer_to_list(Id)).
