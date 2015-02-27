% Copyright (c) 2014-2015, Daniel Kempkens <daniel@kempkens.io>
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

all_message_queues_test() ->
  ?assertMatch(C when is_integer(C), katja_vmstats_metrics:all_message_queues()).

context_switches_test() ->
  ?assertMatch(C when is_integer(C), katja_vmstats_metrics:context_switches()).

error_logger_message_queue_test() ->
  ?assertMatch(C when is_integer(C), katja_vmstats_metrics:error_logger_message_queue()).

ets_count_test() ->
  ?assertMatch(C when C > 0, katja_vmstats_metrics:ets_count()).

ets_limit_test() ->
  ?assertMatch(C when C > 0, katja_vmstats_metrics:ets_limit()).

ets_size_test() ->
  ?assertMatch(0, katja_vmstats_metrics:ets_size(table_does_not_exist)).

ets_size_total_test() ->
  ?assertMatch(C when C > 0, katja_vmstats_metrics:ets_size_total()).

ets_utilization_test() ->
  ?assertMatch(C when is_float(C), katja_vmstats_metrics:ets_utilization()).

exact_reductions_last_call_test() ->
  ?assertMatch(C when C > 0, katja_vmstats_metrics:exact_reductions_last_call()).

exact_reductions_total_test() ->
  ?assertMatch(C when C > 0, katja_vmstats_metrics:exact_reductions_total()).

garbage_collection_runs_test() ->
  ?assertMatch(C when is_integer(C), katja_vmstats_metrics:garbage_collection_runs()).

garbage_collection_words_reclaimed_test() ->
  ?assertMatch(C when is_integer(C), katja_vmstats_metrics:garbage_collection_words_reclaimed()).

heap_size_test() ->
  ?assertMatch(C when C > 0, katja_vmstats_metrics:heap_size(error_logger)).

io_in_test() ->
  ?assertMatch(C when is_integer(C), katja_vmstats_metrics:io_in()).

io_out_test() ->
  ?assertMatch(C when is_integer(C), katja_vmstats_metrics:io_out()).

links_test() ->
  ?assertMatch(C when is_integer(C), katja_vmstats_metrics:links(error_logger)).

loaded_modules_test() ->
  ?assertMatch(C when C > 0, katja_vmstats_metrics:loaded_modules()).

memory_atoms_test() ->
  ?assertMatch(C when C > 0, katja_vmstats_metrics:memory_atoms()).

memory_binaries_test() ->
  ?assertMatch(C when C > 0, katja_vmstats_metrics:memory_binaries()).

memory_ets_test() ->
  ?assertMatch(C when is_integer(C), katja_vmstats_metrics:memory_ets()).

memory_process_test() ->
  ?assertMatch(C when is_integer(C), katja_vmstats_metrics:memory_process(error_logger)).

memory_processes_test() ->
  ?assertMatch(C when C > 0, katja_vmstats_metrics:memory_processes()).

memory_system_test() ->
  ?assertMatch(C when C > 0, katja_vmstats_metrics:memory_system()).

memory_total_test() ->
  TotalMemory = katja_vmstats_metrics:memory_total(),
  SystemMemory = katja_vmstats_metrics:memory_system(),
  ?assertMatch(C when C > 0, TotalMemory),
  ?assert(TotalMemory > SystemMemory).

monitors_test() ->
  ?assertMatch(C when is_integer(C), katja_vmstats_metrics:monitors(error_logger)).

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

reductions_last_call_test() ->
  ?assertMatch(C when C > 0, katja_vmstats_metrics:reductions_last_call()).

reductions_process_test() ->
  ?assertMatch(C when C > 0, katja_vmstats_metrics:reductions_process(error_logger)).

reductions_total_test() ->
  ?assertMatch(C when C > 0, katja_vmstats_metrics:reductions_total()).

registered_processes_test() ->
  ?assertMatch(C when C > 0, katja_vmstats_metrics:registered_processes()).

run_queue_test() ->
  ?assertMatch(C when is_integer(C), katja_vmstats_metrics:run_queue()).

socket_recv_package_count_test() ->
  {ok, Socket} = gen_udp:open(0, [binary, {active, false}]),
  ?assertEqual(0, katja_vmstats_metrics:socket_recv_package_count(Socket)),
  ok = gen_udp:close(Socket),
  ?assertEqual(0, katja_vmstats_metrics:socket_recv_package_count(Socket)).

socket_recv_size_test() ->
  {ok, Socket} = gen_udp:open(0, [binary, {active, false}]),
  ?assertEqual(0, katja_vmstats_metrics:socket_recv_size(Socket)),
  ok = gen_udp:close(Socket),
  ?assertEqual(0, katja_vmstats_metrics:socket_recv_size(Socket)).

socket_send_package_count_test() ->
  {ok, Socket} = gen_udp:open(0, [binary, {active, false}]),
  ?assertEqual(0, katja_vmstats_metrics:socket_send_package_count(Socket)),
  ok = gen_udp:send(Socket, "10.99.99.99", 9001, <<1>>),
  ?assertEqual(1, katja_vmstats_metrics:socket_send_package_count(Socket)),
  ok = gen_udp:close(Socket),
  ?assertEqual(0, katja_vmstats_metrics:socket_send_package_count(Socket)).

socket_send_size_test() ->
  {ok, Socket} = gen_udp:open(0, [binary, {active, false}]),
  ?assertEqual(0, katja_vmstats_metrics:socket_send_size(Socket)),
  ok = gen_udp:send(Socket, "10.99.99.99", 9001, <<"test">>),
  ?assertEqual(4, katja_vmstats_metrics:socket_send_size(Socket)),
  ok = gen_udp:close(Socket),
  ?assertEqual(0, katja_vmstats_metrics:socket_send_size(Socket)).

stack_size_test() ->
  ?assertMatch(C when C > 0, katja_vmstats_metrics:stack_size(error_logger)).
