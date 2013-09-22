-module(faster_csv_naming).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([name/2]).

%% ===================================================================
%% API functions
%% ===================================================================

name(Module, Id) ->
  list_to_atom(atom_to_list(Module) ++ "_"++ integer_to_list(Id)).

-ifdef(TEST).
name_test_() ->
	{"It Generates names for processes",
	check_names()}.
	
check_names() ->
	[?_assertEqual(test_1, faster_csv_naming:name(test,1)),
		?_assertEqual(mymodule_10, faster_csv_naming:name(mymodule,10))].
-endif.