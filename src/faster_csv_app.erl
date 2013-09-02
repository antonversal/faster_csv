-module(faster_csv_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, [Id, ParserWrkCount]) ->
    faster_csv_sup:start_link(Id, ParserWrkCount).

stop(_State) ->
    ok.
