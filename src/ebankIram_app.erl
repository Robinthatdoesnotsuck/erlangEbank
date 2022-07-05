%%%-------------------------------------------------------------------
%% @doc ebankIram public API
%% @end
%%%-------------------------------------------------------------------

-module(ebankIram_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ebankIram_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
