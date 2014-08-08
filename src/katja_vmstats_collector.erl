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
  timer = undefined :: [{atom(), pos_integer(), timer:tref()}] | undefined
}).

% Types

-type state() :: #collector_state{}.

% API
-export([
  start_link/0,
  stop/0,
  collect/1,
  get_timer/1,
  start_timer/2,
  stop_timer/1
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

% @doc Returns a list of all timers registered under the given `Name'.
%      Setting `Name' to `all' will return all registered timers.
-spec get_timer(atom()) -> [{atom(), pos_integer()}].
get_timer(Name) ->
  gen_server:call(?MODULE, {get_timer, Name}).

% @doc Registers a new timer under the given `Name'.<br />
%      `MetricsIntervals' has to be a list like the `collector' configuration option.
-spec start_timer(atom(), [katja_vmstats:collection()]) -> ok.
start_timer(Name, MetricsIntervals) ->
  gen_server:call(?MODULE, {start_timer, Name, MetricsIntervals}).

% @doc Stops all timers registered under the given `Name'.
%      Setting `Name' to `all' will stop all registered timers.
-spec stop_timer(atom()) -> ok.
stop_timer(Name) ->
  gen_server:call(?MODULE, {stop_timer, Name}).

% gen_server

% @hidden
init([]) ->
  Service = application:get_env(katja_vmstats, service, ?DEFAULT_SERVICE),
  DelayCollection = application:get_env(katja_vmstats, delay_collection, ?DEFAULT_DELAY_COLLECTION),
  MetricsIntervals = application:get_env(katja_vmstats, collector, ?DEFAULT_COLLECTOR),
  State = #collector_state{service=Service, timer=[]},
  if
    DelayCollection == 0 ->
      State2 = start_collection_intervals(config, MetricsIntervals, State),
      {ok, State2};
    DelayCollection > 0 ->
      _TRef = erlang:send_after(DelayCollection, self(), {start_collection_intervals, config, MetricsIntervals}),
      {ok, State}
  end.

% @hidden
handle_call({get_timer, all}, _From, #collector_state{timer=Timer}=S) ->
  Timer2 = [{TName, Interval} || {TName, Interval, _TRef} <- Timer],
  {reply, Timer2, S};
handle_call({get_timer, Name}, _From, #collector_state{timer=Timer}=S) ->
  Timer2 = [{TName, Interval} || {TName, Interval, _TRef} <- Timer, Name =:= TName],
  {reply, Timer2, S};
handle_call({start_timer, Name, MetricsIntervals}, _From, State) ->
  State2 = start_collection_intervals(Name, MetricsIntervals, State),
  {reply, ok, State2};
handle_call({stop_timer, all}, _From, #collector_state{timer=Timer}=S) ->
  ok = stop_collection_intervals(Timer),
  S2 = S#collector_state{timer=[]},
  {reply, ok, S2};
handle_call({stop_timer, Name}, _From, #collector_state{timer=Timer}=S) ->
  {TimerStop, TimerNext} = lists:splitwith(fun({TName, _Interval, _Tref}) -> Name =:= TName end, Timer),
  ok = stop_collection_intervals(TimerStop),
  S2 = S#collector_state{timer=TimerNext},
  {reply, ok, S2};
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
handle_info({start_collection_intervals, Name, MetricsIntervals}, State) ->
  State2 = start_collection_intervals(Name, MetricsIntervals, State),
  {noreply, State2};
handle_info({collect, Metrics}, #collector_state{service=Service}=S) ->
  {ok, Events} = create_events(Service, Metrics),
  ok = katja:send_events(Events),
  {noreply, S};
handle_info(_Msg, State) ->
  {noreply, State}.

% @hidden
terminate(normal, #collector_state{timer=Timer}) ->
  ok = stop_collection_intervals(Timer),
  ok.

% @hidden
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

% Private

-spec start_collection_intervals(atom(), [katja_vmstats:collection()], state()) -> state().
start_collection_intervals(Name, MetricsIntervals, State) ->
  lists:foldr(fun(MetricsInterval, #collector_state{timer=Timer}=S) ->
    Interval = noesis_proplists:get_value(interval, MetricsInterval),
    Metrics = noesis_proplists:get_value(metrics, MetricsInterval),
    {ok, TRef} = timer:send_interval(Interval, {collect, Metrics}),
    S#collector_state{timer=[{Name, Interval, TRef} | Timer]}
  end, State, MetricsIntervals).

-spec stop_collection_intervals([{atom(), pos_integer(), timer:tref()}]) -> ok.
stop_collection_intervals(Timer) ->
  _ = [{ok, cancel} = timer:cancel(TRef) || {_Name, _Interval, TRef} <- Timer],
  ok.

-spec create_events(iolist(), [atom()]) -> {ok, [katja:event()]}.
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

% Tests (private functions)

-ifdef(TEST).
start_collection_intervals_test() ->
  State = #collector_state{service="test", timer=[]},
  State2 = start_collection_intervals(test, [[{interval, 10000}, {metrics, [process_count]}]], State),
  ?assertMatch(#collector_state{timer=L} when length(L) == 1, State2),
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
