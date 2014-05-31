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
% @doc The `katja_vmstats_collector' module is responsible for collecting metrics and sending them to Riemann.

-module(katja_vmstats_collector).
-behaviour(gen_server).

-define(DEFAULT_SERVICE, "katja_vmstats").
-define(DEFAULT_COLLECTOR, [
  [
    {interval, 1000},
    {metrics, [
      error_logger_message_queue,
      loaded_modules,
      memory_atoms,
      memory_binaries,
      memory_ets,
      memory_processes,
      memory_system,
      memory_total,
      port_count,
      port_limit,
      port_utilization,
      process_count,
      process_limit,
      process_utilization,
      run_queue
    ]}
  ]
]).

-record(collector_state, {
  service = undefined :: iolist() | undefined,
  trefs = undefined :: [timer:tref()] | undefined
}).

% API
-export([
  start_link/0,
  stop/0,
  collect/1
]).

% gen_server
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

% API

% @doc Starts a collector server process and registers it as `{@module}'.
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% @doc Stops a collector server process.
-spec stop() -> ok.
stop() ->
  gen_server:call(?MODULE, terminate).

% @doc Collects the specified metrics and sends them to Riemann.
-spec collect(atom() | [atom()]) -> ok.
collect(Metric) when is_atom(Metric) ->
  collect([Metric]);
collect(Metrics) ->
  gen_server:cast(?MODULE, {collect, Metrics}).

% gen_server

% @hidden
init([]) ->
  Service = application:get_env(katja_vmstats, service, ?DEFAULT_SERVICE),
  MetricsIntervals = application:get_env(katja_vmstats, collector, ?DEFAULT_COLLECTOR),
  TRefs = lists:map(fun(MetricsInterval) ->
    {interval, Interval} = lists:keyfind(interval, 1, MetricsInterval),
    {metrics, Metrics} = lists:keyfind(metrics, 1, MetricsInterval),
    {ok, TRef} = timer:send_interval(Interval, {collect, Metrics}),
    TRef
  end, MetricsIntervals),
  State = #collector_state{service=Service, trefs=TRefs},
  {ok, State}.

% @hidden
handle_call(terminate, _From, State) ->
  {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

% @hidden
handle_cast({collect, Metrics}, #collector_state{service=Service}=S) ->
  {ok, Events} = create_events(Service, Metrics),
  ok = katja:send_events(Events),
  {noreply, S};
handle_cast(_Msg, State) ->
  {noreply, State}.

% @hidden
handle_info({collect, Metrics}, #collector_state{service=Service}=S) ->
  {ok, Events} = create_events(Service, Metrics),
  ok = katja:send_events(Events),
  {noreply, S};
handle_info(_Msg, State) ->
  {noreply, State}.

% @hidden
terminate(normal, #collector_state{trefs=TRefs}) ->
  _ = [{ok, cancel} = timer:cancel(TRef) || TRef <- TRefs],
  ok.

% @hidden
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

% Private

-spec create_events(iolist(), [atom()]) -> {ok, [katja:event()]}.
create_events(Service, Metrics) ->
  Timestamp = current_timestamp(),
  Events = lists:map(fun(Metric) ->
    MetricService = atom_to_list(Metric),
    MetricValue = katja_vmstats_metrics:Metric(),
    [{service, [Service, " ", MetricService]}, {time, Timestamp}, {tags, ["katja_vmstats"]}, {metric, MetricValue}]
  end, Metrics),
  {ok, Events}.

-spec current_timestamp() -> pos_integer().
current_timestamp() ->
  {MegaSecs, Secs, _MicroSecs} = os:timestamp(),
  MegaSecs * 1000000 + Secs.
