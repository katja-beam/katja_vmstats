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

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(DEFAULT_SERVICE, "katja_vmstats").
-define(DEFAULT_DELAY_COLLECTION, 0).
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

-type state() :: #collector_state{}.

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
%      <ul>
%        <li>`atom()': A function defined and exported in the {@link katja_vmstats_metrics} module.</li>
%        <li>`{iolist(), atom(), [any()]}': A function defined and exported in the {@link katja_vmstats_metrics} module.
%            The first tuple field is the name of the metric, the second and the third field are the <em>FA</em> part of <em>MFA</em>.</li>
%        <li>`{iolist(), module(), atom(), [any()]}': Any function in any module that returns a `number()'.
%            The first tuple field is the name of the metric, the following fields are <em>MFA</em>.</li>
%      </ul>
-spec collect(katja_vmstats:metric() | [katja_vmstats:metric()]) -> ok.
collect(Metric) when is_atom(Metric); is_tuple(Metric) ->
  collect([Metric]);
collect(Metrics) ->
  gen_server:cast(?MODULE, {collect, Metrics}).

% gen_server

% @hidden
init([]) ->
  Service = application:get_env(katja_vmstats, service, ?DEFAULT_SERVICE),
  DelayCollection = application:get_env(katja_vmstats, delay_collection, ?DEFAULT_DELAY_COLLECTION),
  MetricsIntervals = application:get_env(katja_vmstats, collector, ?DEFAULT_COLLECTOR),
  State = #collector_state{service=Service, trefs=[]},
  if
    DelayCollection == 0 ->
      State2 = start_collection_intervals(MetricsIntervals, State),
      {ok, State2};
    DelayCollection > 0 ->
      {ok, _TRef} = timer:send_after(DelayCollection, {start_collection_intervals, MetricsIntervals}),
      {ok, State}
  end.

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
handle_info({start_collection_intervals, MetricsIntervals}, State) ->
  State2 = start_collection_intervals(MetricsIntervals, State),
  {noreply, State2};
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

-spec start_collection_intervals([[{atom(), any()}]], state()) -> state().
start_collection_intervals(MetricsIntervals, State) ->
  lists:foldr(fun(MetricsInterval, #collector_state{trefs=TRefs}=S) ->
    {interval, Interval} = lists:keyfind(interval, 1, MetricsInterval),
    {metrics, Metrics} = lists:keyfind(metrics, 1, MetricsInterval),
    {ok, TRef} = timer:send_interval(Interval, {collect, Metrics}),
    S#collector_state{trefs=[TRef | TRefs]}
  end, State, MetricsIntervals).

-spec create_events(iolist(), [atom()]) -> {ok, [katja:event()]}.
create_events(Service, Metrics) ->
  Timestamp = current_timestamp(),
  Events = lists:map(fun(Metric) ->
    MetricService = get_metric_service(Service, Metric),
    MetricValue = get_metric_value(Metric),
    [{service, MetricService}, {time, Timestamp}, {tags, ["katja_vmstats"]}, {metric, MetricValue}]
  end, Metrics),
  {ok, Events}.

-spec current_timestamp() -> pos_integer().
current_timestamp() ->
  {MegaSecs, Secs, _MicroSecs} = os:timestamp(),
  MegaSecs * 1000000 + Secs.

-spec get_metric_service(iolist(), katja_vmstats:metric()) -> iolist().
get_metric_service(BaseService, Metric) when is_atom(Metric) ->
  Metric2 = atom_to_list(Metric),
  [BaseService, " ", Metric2];
get_metric_service(BaseService, {Service, _Fun, _Args}) ->
  [BaseService, " ", Service];
get_metric_service(BaseService, {Service, _Mod, _Fun, _Args}) ->
  [BaseService, " ", Service].

-spec get_metric_value(katja_vmstats:metric()) -> number().
get_metric_value(Metric) when is_atom(Metric) ->
  katja_vmstats_metrics:Metric();
get_metric_value({_Service, Fun, Args}) ->
  apply(katja_vmstats_metrics, Fun, Args);
get_metric_value({_Service, Mod, Fun, Args}) ->
  apply(Mod, Fun, Args).

% Tests (private functions)

-ifdef(TEST).
start_collection_intervals_test() ->
  State = #collector_state{service="test", trefs=[]},
  State2 = start_collection_intervals([[{interval, 10000}, {metrics, [process_count]}]], State),
  ?assertMatch(#collector_state{trefs=L} when length(L) == 1, State2),
  ?assertEqual(ok, terminate(normal, State2)).

get_metric_service_test() ->
  ?assertEqual(["katja_vmstats", " ", "test"], get_metric_service("katja_vmstats", test)),
  ?assertEqual(["katja_vmstats", " ", "friday"], get_metric_service("katja_vmstats", {"friday", funfunfun, []})),
  ?assertEqual(["katja_vmstats", " ", "friday"], get_metric_service("katja_vmstats", {"friday", rebecca, funfunfun, []})).

get_metric_value_test() ->
  ProcessCount = erlang:system_info(process_count),
  ?assertEqual(ProcessCount, get_metric_value(process_count)),
  ?assertEqual(ProcessCount, get_metric_value({"process_count", process_count, []})),
  ?assertEqual(ProcessCount, get_metric_value({"process_count", katja_vmstats_metrics, process_count, []})),
  ?assertEqual(ProcessCount, get_metric_value({"process_count", erlang, system_info, [process_count]})).
-endif.
