% Copyright (c) 2014, Daniel Kempkens <daniel@kempkens.io>
%
% Permission to use, copy, modify, and/or distribute this software for any purpose with or without fee is hereby granted,
% provided that the above copyright notice and this permission notice appear in all copies.
%
% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
% WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
% DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
% NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(katja_vmstats_metrics_test).

-include_lib("eunit/include/eunit.hrl").

error_logger_message_queue_test() ->
  ?assertMatch(C when is_integer(C), katja_vmstats_metrics:error_logger_message_queue()).

ets_count_test() ->
  ?assertMatch(C when C > 0, katja_vmstats_metrics:ets_count()).

ets_limit_test() ->
  ?assertMatch(C when C > 0, katja_vmstats_metrics:ets_limit()).

ets_utilization_test() ->
  ?assertMatch(C when is_float(C), katja_vmstats_metrics:ets_utilization()).

loaded_modules_test() ->
  ?assertMatch(C when C > 0, katja_vmstats_metrics:loaded_modules()).

memory_atoms_test() ->
  ?assertMatch(C when C > 0, katja_vmstats_metrics:memory_atoms()).

memory_binaries_test() ->
  ?assertMatch(C when C > 0, katja_vmstats_metrics:memory_binaries()).

memory_ets_test() ->
  ?assertMatch(C when is_integer(C), katja_vmstats_metrics:memory_ets()).

memory_processes_test() ->
  ?assertMatch(C when C > 0, katja_vmstats_metrics:memory_processes()).

memory_system_test() ->
  ?assertMatch(C when C > 0, katja_vmstats_metrics:memory_system()).

memory_total_test() ->
  TotalMemory = katja_vmstats_metrics:memory_total(),
  SystemMemory = katja_vmstats_metrics:memory_system(),
  ?assertMatch(C when C > 0, TotalMemory),
  ?assert(TotalMemory > SystemMemory).

port_count_test() ->
  ?assertMatch(C when C > 0, katja_vmstats_metrics:port_count()).

port_limit_test() ->
  ?assertMatch(C when C > 0, katja_vmstats_metrics:port_limit()).

port_utilization_test() ->
  ?assertMatch(C when is_float(C), katja_vmstats_metrics:port_utilization()).

process_count_test() ->
  ?assertMatch(C when C > 0, katja_vmstats_metrics:process_count()).

process_limit_test() ->
  ?assertMatch(C when C > 0, katja_vmstats_metrics:process_limit()).

process_utilization_test() ->
  ?assertMatch(C when is_float(C), katja_vmstats_metrics:process_utilization()).

run_queue_test() ->
  ?assertMatch(C when is_integer(C), katja_vmstats_metrics:run_queue()).
