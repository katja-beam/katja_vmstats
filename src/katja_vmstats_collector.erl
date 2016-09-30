% Copyright (c) 2014-2016, Daniel Kempkens <daniel@kempkens.io>
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
-define(DEFAULT_TRANSPORT, config).
-define(DEFAULT_SEND_ASYNC, false).
-define(DEFAULT_ASYNC_SAMPLE_RATE, 1.0).
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
  service :: iolist(),
  transport :: katja_connection:transport(),
  collections :: [collection()]
}).

-record(collection, {
  tref :: timer:tref(),
  name :: atom(),
  metrics :: [katja_vmstats:metric()],
  interval :: pos_integer(),
  send_async :: boolean(),
  async_sample_rate :: float()
}).

% Types

-type state() :: #collector_state{}.
-type collection() :: #collection{}.

% API
-export([
  start_link/0,
  stop/0,
  collect/1,
  get_collection/1,
  start_collection/2,
  stop_collection/1
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
-spec collect([katja_vmstats:metric()]) -> ok.
collect(Metrics) ->
  gen_server:cast(?MODULE, {collect, Metrics}).

% @doc Returns a list of all collections registered under the given `Name'.
%      Setting `Name' to `all' will return all registered collections.
-spec get_collection(atom()) -> [katja_vmstats:collection()].
get_collection(Name) ->
  gen_server:call(?MODULE, {get_collection, Name}).

% @doc Registers a new collection under the given `Name'.<br />
%      `MetricsIntervals' has to be a list like the `collector' configuration option.
-spec start_collection(atom(), [katja_vmstats:collection()]) -> ok.
start_collection(Name, MetricsIntervals) ->
  gen_server:call(?MODULE, {start_collection, Name, MetricsIntervals}).

% @doc Stops all collections registered under the given `Name'.
%      Setting `Name' to `all' will stop all registered collections.
-spec stop_collection(atom()) -> ok.
stop_collection(Name) ->
  gen_server:call(?MODULE, {stop_collection, Name}).

% gen_server

% @hidden
init([]) ->
  Service = application:get_env(katja_vmstats, service, ?DEFAULT_SERVICE),
  Transport = application:get_env(katja_vmstats, transport, ?DEFAULT_TRANSPORT),
  DelayCollection = application:get_env(katja_vmstats, delay_collection, ?DEFAULT_DELAY_COLLECTION),
  MetricsIntervals = application:get_env(katja_vmstats, collector, ?DEFAULT_COLLECTOR),
  State = #collector_state{service=Service, transport=Transport, collections=[]},
  if
    DelayCollection == 0 ->
      State2 = start_collection_intervals(config, MetricsIntervals, State),
      {ok, State2};
    DelayCollection > 0 ->
      _TRef = erlang:send_after(DelayCollection, self(), {start_collection_intervals, config, MetricsIntervals}),
      {ok, State}
  end.

% @hidden
handle_call({get_collection, all}, _From, #collector_state{collections=Collections}=S) ->
  Collection = [collection_to_proplist(C) || C <- Collections],
  {reply, Collection, S};
handle_call({get_collection, Name}, _From, #collector_state{collections=Collections}=S) ->
  Collection = [collection_to_proplist(C) || C <- Collections, Name =:= C#collection.name],
  {reply, Collection, S};
handle_call({start_collection, Name, MetricsIntervals}, _From, State) ->
  State2 = start_collection_intervals(Name, MetricsIntervals, State),
  {reply, ok, State2};
handle_call({stop_collection, all}, _From, #collector_state{collections=Collections}=S) ->
  ok = stop_collection_intervals(Collections),
  S2 = S#collector_state{collections=[]},
  {reply, ok, S2};
handle_call({stop_collection, Name}, _From, #collector_state{collections=Collections}=S) ->
  {CollectionStop, CollectionNext} = lists:splitwith(fun(#collection{name=TName}) -> Name =:= TName end, Collections),
  ok = stop_collection_intervals(CollectionStop),
  S2 = S#collector_state{collections=CollectionNext},
  {reply, ok, S2};
handle_call(terminate, _From, State) ->
  {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

% @hidden
handle_cast({collect, Metrics}, State) ->
  SendAsync = application:get_env(katja_vmstats, send_async, ?DEFAULT_SEND_ASYNC),
  AsyncSampleRate = application:get_env(katja_vmstats, async_sample_rate, ?DEFAULT_ASYNC_SAMPLE_RATE),
  Collection = #collection{metrics=Metrics, send_async=SendAsync, async_sample_rate=AsyncSampleRate},
  ok = collect_events(Collection, State),
  {noreply, State};
handle_cast(_Msg, State) ->
  {noreply, State}.

% @hidden
handle_info({start_collection_intervals, Name, MetricsIntervals}, State) ->
  State2 = start_collection_intervals(Name, MetricsIntervals, State),
  {noreply, State2};
handle_info({collect, Metrics}, State) ->
  ok = collect_events(Metrics, State),
  {noreply, State};
handle_info(_Msg, State) ->
  {noreply, State}.

% @hidden
terminate(normal, #collector_state{collections=Collections}) ->
  ok = stop_collection_intervals(Collections),
  ok.

% @hidden
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

% Private

-spec start_collection_intervals(atom(), [katja_vmstats:collection()], state()) -> state().
start_collection_intervals(Name, MetricsIntervals, State) ->
  ConfigSendAsync = application:get_env(katja_vmstats, send_async, ?DEFAULT_SEND_ASYNC),
  ConfigAsyncSampleRate = application:get_env(katja_vmstats, async_sample_rate, ?DEFAULT_ASYNC_SAMPLE_RATE),
  lists:foldr(fun(MetricsInterval, #collector_state{collections=Collections}=S) ->
    Interval = noesis_proplists:get_value(interval, MetricsInterval),
    SendAsync = noesis_proplists:get_value(send_async, MetricsInterval, ConfigSendAsync),
    AsyncSampleRate = noesis_proplists:get_value(async_sample_rate, MetricsInterval, ConfigAsyncSampleRate),
    Metrics = noesis_proplists:get_value(metrics, MetricsInterval),
    NewCollection = #collection{name=Name, metrics=Metrics, interval=Interval, send_async=SendAsync, async_sample_rate=AsyncSampleRate},
    {ok, TRef} = timer:send_interval(Interval, {collect, NewCollection}),
    NewCollection2 = NewCollection#collection{tref=TRef},
    S#collector_state{collections=[NewCollection2 | Collections]}
  end, State, MetricsIntervals).

-spec stop_collection_intervals([collection()]) -> ok.
stop_collection_intervals(Collections) ->
  _ = [{ok, cancel} = timer:cancel(C#collection.tref) || C <- Collections],
  ok.

-spec collect_events(collection(), state()) -> ok | {error, term()}.
collect_events(#collection{metrics=Metrics, send_async=SendAsync, async_sample_rate=AsyncSampleRate}, #collector_state{service=Service, transport=Transport}) ->
  {ok, Events} = create_events(Service, Metrics),
  if
    SendAsync -> katja:send_events_async(katja_writer, Transport, Events, AsyncSampleRate);
    true -> katja:send_events(katja_writer, Transport, Events)
  end.

-spec create_events(iolist(), [katja_vmstats:metric()]) -> {ok, [katja:event()]}.
create_events(Service, Metrics) ->
  Timestamp = noesis_datetime:timestamp(),
  Events = noesis_lists:pmap(fun(Metric) ->
    MetricService = get_metric_service(Service, Metric),
    MetricValue = get_metric_value(Metric),
    [{service, MetricService}, {time, Timestamp}, {tags, ["katja_vmstats"]}, {metric, MetricValue}]
  end, Metrics),
  {ok, Events}.

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

-spec collection_to_proplist(collection()) -> noesis_proplists:proplist(atom(), term()).
collection_to_proplist(Collection) ->
  Fields = record_info(fields, collection),
  [collection|Values] = tuple_to_list(Collection),
  noesis_proplists:delete_keys([tref], lists:zip(Fields, Values)).

% Tests (private functions)

-ifdef(TEST).
start_collection_intervals_test() ->
  State = #collector_state{service="test", collections=[]},
  State2 = start_collection_intervals(test, [[{interval, 10000}, {metrics, [process_count]}]], State),
  ?assertMatch(#collector_state{collections=L} when length(L) == 1, State2),
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
