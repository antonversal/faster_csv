-module(basic_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([receiving_lines/1, receiving_parsers_are_done/1]).

all() ->
	[receiving_lines,
	receiving_parsers_are_done].

init_per_suite(Config) ->
	Pid = spawn(fun() -> receive stop -> ok end end),
	faster_csv_sup:start_link(1, 4, Pid, $,, $"),
	FileName = string:concat(filename:dirname(code:which(?MODULE)), "/fixtures/test.csv"),
	faster_csv_reader:read(1, FileName),
	ct:sleep(10),
	[{receiver, Pid} | Config].

end_per_suite(Config) ->
	?config(receiver, Config) ! stop.

receiving_lines(Config) ->
	{messages, Messages} = erlang:process_info(?config(receiver, Config), messages),
	Line1 = {1,["managerID","awardID","yearID","lgID","tie","notes"]},
	true = lists:member(Line1, Messages),
	Line2 = {2,["larusto01m","BBWAA Manager of the year","1983","AL", nil, nil]},
	true = lists:member(Line2, Messages),
	Line3 = {3,["lasorto01m","BBWAA Manager of the year","1983","NL","1","1"]},
	true = lists:member(Line3, Messages),
	Line4 = {4,["andersp01m","BBWAA Manager of the year","1984","AL","1",nil]},
	true = lists:member(Line4, Messages).

receiving_parsers_are_done(Config) ->
	{messages, Messages} = erlang:process_info(?config(receiver, Config), messages),
	4 = length(lists:filter(fun(Elem) -> 
														element(1, Elem) == parser_is_done
													end,Messages)).