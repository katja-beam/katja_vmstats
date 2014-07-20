% Copyright (c) 2014, Daniel Kempkens <daniel@kempkens.io>
%
% Permission to use, copy, modify, and/or distribute this software for any purpose with or without fee is hereby granted,
% provided that the above copyright notice and this permission notice appear in all copies.
%
% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
% WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
% DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
% NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
%
% @author Daniel Kempkens <daniel@kempkens.io>
% @copyright {@years} Daniel Kempkens
% @version {@version}
% @doc This is the main module of the Katja VM Stats application: It provides the public API.<br />
%      While it is possible to use `katja_vmstats_collector' directly, the recommended way is to use the functions defined
%      in this module instead.

-module(katja_vmstats).

% Types

-type metric() :: atom() | {iolist(), atom(), [any()]} | {iolist(), module(), atom(), [any()]}.
-type collection() :: [{interval, pos_integer()} | {metrics, metric()}].

-export_type([
  metric/0,
  collection/0
]).

% API
-export([
  start/0,
  collect/1,
  get_timer/1,
  start_timer/2,
  stop_timer/1
]).

% API

% @doc Starts the Katja VM Stats application and all of its dependencies. This is really only meant for usage inside the console.
-spec start() -> ok.
start() ->
  ok = application:start(protobuffs),
  ok = application:start(katja),
  ok = application:start(katja_vmstats),
  ok.

% @doc Collects the specified metrics and sends them to Riemann. Delegates to {@link katja_vmstats_collector:collect/1}.
-spec collect(metric() | [metric()]) -> ok.
collect(Metric) when is_atom(Metric); is_tuple(Metric) ->
  collect([Metric]);
collect(Metrics) ->
  katja_vmstats_collector:collect(Metrics).

% @doc Returns a list of all timers registered under the given `Name'. Delegates to {@link katja_vmstats_collector:get_timer/1}.
-spec get_timer(atom()) -> [{atom(), pos_integer()}].
get_timer(Name) ->
  katja_vmstats_collector:get_timer(Name).

% @doc Registers a new timer under the given `Name'. Delegates to {@link katja_vmstats_collector:start_timer/2}.
-spec start_timer(atom(), katja_vmstats:collection() | [katja_vmstats:collection()]) -> ok.
start_timer(Name, MetricsIntervals) when is_tuple(hd(MetricsIntervals)) ->
  start_timer(Name, [MetricsIntervals]);
start_timer(Name, MetricsIntervals) ->
  katja_vmstats_collector:start_timer(Name, MetricsIntervals).

% @doc Stops all timers registered under the given `Name'. Delegates to {@link katja_vmstats_collector:stop_timer/1}.
-spec stop_timer(atom()) -> ok.
stop_timer(Name) ->
  katja_vmstats_collector:stop_timer(Name).
