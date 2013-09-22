-module(basic_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([receiving_lines/1]).

all() ->
	[receiving_lines].

receiving_lines(_Config) ->
	faster_csv_sup:start_link(1, 4, self()),
	FileName = string:concat(filename:dirname(code:which(?MODULE)), "/fixtures/test.csv"),
	io:format("File is in ~s ~n", [FileName]),
	faster_csv_reader:read(1, FileName),
	ct:sleep(10),
	{messages, Messages} = erlang:process_info(self(), messages),
	io:format("Messages ~p ~n", [Messages]),
	[{1, Line1}|Tail1] = Messages,
	io:format("Line 1 ~p ~n", [Line1]),
	["managerID","awardID","yearID","lgID","tie","notes"] = Line1,
	[{2, Line2}|Tail2] = Tail1,
	io:format("Line 2 ~p ~n", [Line2]),
	["larusto01m","BBWAA Manager of the year","1983","AL", nil, nil] = Line2,
	[{3, Line3}|Tail3] = Tail2,
	io:format("Line 3 ~p ~n", [Line3]),
	["lasorto01m","BBWAA Manager of the year","1983","NL","1","1"]= Line3,
	[{4, Line4}|_] = Tail3,
	io:format("Line 4 ~p ~n", [Line4]),
	["andersp01m","BBWAA Manager of the year","1984","AL","1",nil] = Line4.	
	
	


