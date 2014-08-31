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
% This suite expects Riemann to be running on 127.0.0.1:5555.
% Since events from <em>katja_vmstats</em> have to indexed in order to be queried, the following configuration
% option is required:
%   (tagged "katja_vmstats" index)

-module(collector_SUITE).

-include_lib("common_test/include/ct.hrl").

% Common Test
-export([
  all/0,
  groups/0,
  init_per_group/2,
  end_per_group/2
]).

% Tests
-export([
  config_events/1,
  manual_events/1,
  programmatic_collections/1,
  ignore_unknown_messages/1
]).

% Common Test

all() ->
  [
    {group, without_delay},
    {group, with_delay}
  ].

groups() ->
  [
    {without_delay, [sequence], [
      config_events,
      manual_events,
      programmatic_collections,
      ignore_unknown_messages
    ]},
    {with_delay, [sequence], [
      config_events
    ]}
  ].

init_per_group(without_delay, Config) ->
  ok = katja_vmstats:start(),
  [{manual_delay, 2500}, {auto_delay, 2500} | Config];
init_per_group(with_delay, Config) ->
  ok = application:set_env(katja_vmstats, send_async, true),
  ok = application:set_env(katja_vmstats, delay_collection, 5000),
  ok = katja_vmstats:start(),
  [{manual_delay, 3000}, {auto_delay, 6500} | Config].

end_per_group(_Group, _Config) ->
  ok = katja_vmstats:stop(),
  ok.

% Tests

config_events(Config) ->
  ok = timer:sleep(?config(auto_delay, Config)), % Wait a bit, so that the timer can actually send stuff
  ProcessLimit = katja_vmstats_metrics:process_limit(),
  {ok, [ProcessEvent]} = katja:query_event([{service, "katja_vmstats process_limit"}]),
  {metric, ProcessLimit} = lists:keyfind(metric, 1, ProcessEvent).

manual_events(Config) ->
  EtsCount = katja_vmstats_metrics:ets_count(),
  EtsLimit = katja_vmstats_metrics:ets_limit(),
  ProcessLimit = katja_vmstats_metrics:process_limit(),
  ok = katja_vmstats:collect(ets_count),
  ok = katja_vmstats:collect([ets_limit]),
  ok = katja_vmstats:collect({"tuple_process_limit", erlang, system_info, [process_limit]}),
  ok = timer:sleep(?config(manual_delay, Config)), % Wait a bit, so that we can actually send stuff
  {ok, [EtsCountEvent]} = katja:query_event([{service, "katja_vmstats ets_count"}]),
  {metric, EtsCount} = lists:keyfind(metric, 1, EtsCountEvent),
  {ok, [EtsLimitEvent]} = katja:query_event([{service, "katja_vmstats ets_limit"}]),
  {metric, EtsLimit} = lists:keyfind(metric, 1, EtsLimitEvent),
  {ok, [ProcessLimitEvent]} = katja:query_event([{service, "katja_vmstats tuple_process_limit"}]),
  {metric, ProcessLimit} = lists:keyfind(metric, 1, ProcessLimitEvent).

programmatic_collections(Config) ->
  1 = length(katja_vmstats:get_collection(all)),
  1 = length(katja_vmstats:get_collection(config)),
  ok = katja_vmstats:start_collection(test, [
    [{interval, 1000}, {metrics, [{"reductions_process", reductions_process, [katja_vmstats_collector]}]}],
    [{interval, 1000}, {send_async, true}, {async_sample_rate, 0.5}, {metrics, [{"memory_process", memory_process, [katja_vmstats_collector]}]}]
  ]),
  3 = length(katja_vmstats:get_collection(all)),
  2 = length(katja_vmstats:get_collection(test)),
  ok = timer:sleep(?config(manual_delay, Config)), % Wait a bit, so that the timer can actually send stuff
  {ok, [_]} = katja:query_event([{service, "katja_vmstats reductions_process"}]),
  ok = katja_vmstats:stop_collection(test),
  1 = length(katja_vmstats:get_collection(all)),
  ok = katja_vmstats:stop_collection(all),
  0 = length(katja_vmstats:get_collection(all)).

ignore_unknown_messages(_Config) ->
  ignored = gen_server:call(katja_vmstats_collector, foobar),
  ok = gen_server:cast(katja_vmstats_collector, foobar),
  katja_vmstats_collector ! foobar.
