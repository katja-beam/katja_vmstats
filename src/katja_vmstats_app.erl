%%%-------------------------------------------------------------------
%% @doc katja_vmstats public API
%% @end
%%%-------------------------------------------------------------------

-module(katja_vmstats_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    katja_vmstats_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
