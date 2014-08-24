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

-type collection_name() :: {name, atom()}.
-type collection_metrics() :: {metrics, [metric()]}.
-type collection_interval() :: {interval, pos_integer()}.
-type collection_send_async() :: {send_async, boolean()}.
-type collection_async_sample_rate() :: {async_sample_rate, float()}.

-type collection_attributes() :: collection_name() | collection_metrics() | collection_interval() |
                                 collection_send_async() | collection_async_sample_rate().

-type metric() :: atom() | {iolist(), atom(), [any()]} | {iolist(), module(), atom(), [any()]}.
-type collection() :: [collection_attributes()].

-export_type([
  metric/0,
  collection/0
]).

% API
-export([
  start/0,
  stop/0,
  collect/1,
  get_collection/1,
  start_collection/2,
  stop_collection/1
]).

% API

% @doc Starts the Katja VM Stats application and all of its dependencies. This is really only meant for usage inside the console.
-spec start() -> ok.
start() ->
  ok = application:start(noesis),
  ok = application:start(protobuffs),
  ok = application:start(katja),
  ok = application:start(katja_vmstats),
  ok.

% @doc Stops the Katja VM Stats application and all of its dependencies. This is really only meant for usage inside the console.
-spec stop() -> ok.
stop() ->
  ok = application:stop(katja_vmstats),
  ok = application:stop(katja),
  ok = application:stop(protobuffs),
  ok = application:stop(noesis),
  ok.

% @doc Collects the specified metrics and sends them to Riemann. Delegates to {@link katja_vmstats_collector:collect/1}.
-spec collect(metric() | [metric()]) -> ok.
collect(Metric) when is_atom(Metric); is_tuple(Metric) ->
  collect([Metric]);
collect(Metrics) ->
  katja_vmstats_collector:collect(Metrics).

% @doc Returns a list of all collections registered under the given `Name'. Delegates to {@link katja_vmstats_collector:get_collection/1}.
-spec get_collection(atom()) -> [katja_vmstats:collection()].
get_collection(Name) ->
  katja_vmstats_collector:get_collection(Name).

% @doc Registers a new collection under the given `Name'. Delegates to {@link katja_vmstats_collector:start_collection/2}.
-spec start_collection(atom(), katja_vmstats:collection() | [katja_vmstats:collection()]) -> ok.
start_collection(Name, MetricsIntervals) when is_tuple(hd(MetricsIntervals)) ->
  start_collection(Name, [MetricsIntervals]);
start_collection(Name, MetricsIntervals) ->
  katja_vmstats_collector:start_collection(Name, MetricsIntervals).

% @doc Stops all collections registered under the given `Name'. Delegates to {@link katja_vmstats_collector:stop_collection/1}.
-spec stop_collection(atom()) -> ok.
stop_collection(Name) ->
  katja_vmstats_collector:stop_collection(Name).
