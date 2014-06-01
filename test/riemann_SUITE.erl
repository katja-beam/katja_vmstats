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

-module(riemann_SUITE).

-include_lib("common_test/include/ct.hrl").

% Common Test
-export([
  all/0,
  init_per_suite/1,
  end_per_suite/1
]).

% Tests
-export([
  timer_events/1,
  manual_events/1
]).

% Common Test

all() ->
  [
    timer_events,
    manual_events
  ].

init_per_suite(Config) ->
  ok = application:start(katja),
  ok = application:start(katja_vmstats),
  Config.

end_per_suite(_Config) ->
  ok = application:stop(katja_vmstats),
  ok = application:stop(katja),
  ok.

% Tests

timer_events(_Config) ->
  ok = timer:sleep(2000), % Wait for two seconds, so that the timer can actually sent stuff
  ProcessLimit = katja_vmstats_metrics:process_limit(),
  {ok, [ProcessEvent]} = katja:query_event([{service, "katja_vmstats process_limit"}]),
  {metric, ProcessLimit} = lists:keyfind(metric, 1, ProcessEvent).

manual_events(_Config) ->
  EtsCount = katja_vmstats_metrics:ets_count(),
  EtsLimit = katja_vmstats_metrics:ets_limit(),
  ProcessLimit = katja_vmstats_metrics:process_limit(),
  ok = katja_vmstats:collect(ets_count),
  ok = katja_vmstats:collect([ets_limit]),
  ok = katja_vmstats:collect({"tuple_process_limit", erlang, system_info, [process_limit]}),
  {ok, [EtsCountEvent]} = katja:query_event([{service, "katja_vmstats ets_count"}]),
  {metric, EtsCount} = lists:keyfind(metric, 1, EtsCountEvent),
  {ok, [EtsLimitEvent]} = katja:query_event([{service, "katja_vmstats ets_limit"}]),
  {metric, EtsLimit} = lists:keyfind(metric, 1, EtsLimitEvent),
  {ok, [ProcessLimitEvent]} = katja:query_event([{service, "katja_vmstats tuple_process_limit"}]),
  {metric, ProcessLimit} = lists:keyfind(metric, 1, ProcessLimitEvent).
