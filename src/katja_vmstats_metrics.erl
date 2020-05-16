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
% @doc The `katja_vmstats_metrics' module implements all available metrics.

-module(katja_vmstats_metrics).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

% API
-export([
  all_message_queues/0,
  context_switches/0,
  error_logger_message_queue/0,
  ets_count/0,
  ets_limit/0,
  ets_size/1,
  ets_size_total/0,
  ets_utilization/0,
  exact_reductions_last_call/0,
  exact_reductions_total/0,
  garbage_collection_runs/0,
  garbage_collection_words_reclaimed/0,
  heap_size/1,
  io_in/0,
  io_out/0,
  links/1,
  loaded_modules/0,
  memory_atoms/0,
  memory_binaries/0,
  memory_ets/0,
  memory_process/1,
  memory_processes/0,
  memory_system/0,
  memory_total/0,
  message_queue/1,
  monitors/1,
  port_count/0,
  port_limit/0,
  port_utilization/0,
  process_count/0,
  process_limit/0,
  process_utilization/0,
  reductions_last_call/0,
  reductions_process/1,
  reductions_total/0,
  registered_processes/0,
  run_queue/0,
  socket_recv_package_count/1,
  socket_recv_size/1,
  socket_send_package_count/1,
  socket_send_size/1,
  stack_size/1
]).

% API

% @doc Returns the (summed) size of all message queues at the local node.
-spec all_message_queues() -> non_neg_integer().
all_message_queues() ->
  Processes = processes(),
  lists:foldl(fun(Pid, Acc) ->
    Size = message_queue(Pid),
    Size + Acc
  end, 0, Processes).

% @doc Returns the total number of context switches since the system started.
-spec context_switches() -> non_neg_integer().
context_switches() ->
  {ContextSwitches, 0} = erlang:statistics(context_switches),
  ContextSwitches.

% @doc Returns the size of the `error_logger' message queue at the local node.
% @deprecated
-spec error_logger_message_queue() -> non_neg_integer().
error_logger_message_queue() ->
  message_queue(error_logger).

% @doc Returns the number of ETS tables currently existing at the local node.
-spec ets_count() -> pos_integer().
ets_count() ->
  length(ets:all()).

% @doc Returns the maximum number of ETS tables allowed.<br />
%      On older R16 releases `ets_limit' falls back to reading the `ERL_MAX_ETS_TABLES' environment variable or defaults to 1400.
-spec ets_limit() -> pos_integer().
ets_limit() ->
  try erlang:system_info(ets_limit) of
    Limit -> Limit
  catch
    error:badarg -> r16_ets_limit()
  end.

% @doc Returns the number of objects inserted in the table.
-spec ets_size(ets:tab()) -> non_neg_integer().
ets_size(Tab) ->
  case ets:info(Tab, size) of
    undefined -> 0;
    Size -> Size
  end.

% @doc Returns the total number of objects inserted into all tables.
-spec ets_size_total() -> non_neg_integer().
ets_size_total() ->
  Tables = ets:all(),
  lists:foldl(fun(Tab, Acc) ->
    Size = ets_size(Tab),
    Size + Acc
  end, 0, Tables).

% @doc Returns the ETS table utilization (number between 0 and 1) at the local node.
-spec ets_utilization() -> float().
ets_utilization() ->
  ets_count() / ets_limit().

% @doc Returns the exact number of reductions performed since the last call to this function.<br />
%      Using this method and {@link exact_reductions_total/0} at the same time will cause weird/wrong results.<br />
%      This method is more expensive than {@link reductions_last_call/0}.
-spec exact_reductions_last_call() -> pos_integer().
exact_reductions_last_call() ->
  {_Total, LastCall} = erlang:statistics(exact_reductions),
  LastCall.

% @doc Returns the exact total number of reductions performed at he local node.<br />
%      Using this method and {@link exact_reductions_last_call/0} at the same time will cause weird/wrong results.<br />
%      This method is more expensive than {@link reductions_total/0}.
-spec exact_reductions_total() -> pos_integer().
exact_reductions_total() ->
  {Total, _LastCall} = erlang:statistics(exact_reductions),
  Total.

% @doc Returns the total number of garbage collector runs.
-spec garbage_collection_runs() -> non_neg_integer().
garbage_collection_runs() ->
  {Runs, _WordsReclaimed, 0} = erlang:statistics(garbage_collection),
  Runs.

% @doc Returns the total number of words reclaimed by the garbage collector.
-spec garbage_collection_words_reclaimed() -> non_neg_integer().
garbage_collection_words_reclaimed() ->
  {_Runs, WordsReclaimed, 0} = erlang:statistics(garbage_collection),
  WordsReclaimed.

% @doc Returns the size in words of youngest heap generation of the process.
%      This generation currently include the stack of the process.<br />
%      If `Pid' is an `atom()', it will assume that it's the name of a registered process.
-spec heap_size(atom() | pid()) -> pos_integer().
heap_size(Pid) ->
  process_info_field(Pid, heap_size).

% @doc Returns the total number of bytes received through ports.
-spec io_in() -> non_neg_integer().
io_in() ->
  {{input, Input}, {output, _Output}} = erlang:statistics(io),
  Input.

% @doc Returns the total number of bytes output to ports.
-spec io_out() -> non_neg_integer().
io_out() ->
  {{input, _Input}, {output, Output}} = erlang:statistics(io),
  Output.

% @doc Returns the number of processes or ports to which the process has a link.
%      If `Pid' is an `atom()', it will assume that it's the name of a registered process.
-spec links(atom() | pid()) -> non_neg_integer().
links(Pid) ->
  process_info_field(Pid, links).

% @doc Returns the number of currently loaded modules at the local node.
-spec loaded_modules() -> pos_integer().
loaded_modules() ->
  length(code:all_loaded()).

% @doc The total amount of memory currently used for atoms.
-spec memory_atoms() -> pos_integer().
memory_atoms() ->
  erlang:memory(atom_used).

% @doc The total amount of memory currently used for binaries.
-spec memory_binaries() -> pos_integer().
memory_binaries() ->
  erlang:memory(binary).

% @doc The total amount of memory currently used for ETS tables.
-spec memory_ets() -> pos_integer().
memory_ets() ->
  erlang:memory(ets).

% @doc Size of the process in bytes. This includes call stack, heap and internal structures.
%      If `Pid' is an `atom()', it will assume that it's the name of a registered process.
-spec memory_process(atom() | pid()) -> non_neg_integer().
memory_process(Pid) ->
  process_info_field(Pid, memory).

% @doc The total amount of memory currently used by the Erlang processes.
-spec memory_processes() -> pos_integer().
memory_processes() ->
  erlang:memory(processes_used).

% @doc The total amount of memory currently allocated by the emulator that is not directly related to any Erlang process.
-spec memory_system() -> pos_integer().
memory_system() ->
  erlang:memory(system).

% @doc The total amount of memory currently allocated.
-spec memory_total() -> pos_integer().
memory_total() ->
  erlang:memory(total).

% @doc Returns the size of the message queue of the process identified by `Pid'.
%      If `Pid' is an `atom()', it will assume that it's the name of a registered process.
-spec message_queue(atom() | pid()) -> non_neg_integer().
message_queue(Pid) ->
  process_info_field(Pid, message_queue_len).

% @doc Returns the number of monitors that are active for the process.
%      If `Pid' is an `atom()', it will assume that it's the name of a registered process.
-spec monitors(atom() | pid()) -> non_neg_integer().
monitors(Pid) ->
  process_info_field(Pid, monitors).

% @doc Returns the number of ports currently existing at the local node.
-spec port_count() -> pos_integer().
port_count() ->
  erlang:system_info(port_count).

% @doc Returns the maximum number of simultaneously existing ports at the local node.
-spec port_limit() -> pos_integer().
port_limit() ->
  erlang:system_info(port_limit).

% @doc Returns the port utilization (number between 0 and 1) at the local node.
-spec port_utilization() -> float().
port_utilization() ->
  port_count() / port_limit().

% @doc Returns the number of processes currently existing at the local node.
-spec process_count() -> pos_integer().
process_count() ->
  erlang:system_info(process_count).

% @doc Returns the maximum number of simultaneously existing processes at the local node.
-spec process_limit() -> pos_integer().
process_limit() ->
  erlang:system_info(process_limit).

% @doc Returns the process utilization (number between 0 and 1) at the local node.
-spec process_utilization() -> float().
process_utilization() ->
  process_count() / process_limit().

% @doc Returns the number of reductions (minus reductions performed in current time slices
%      of currently scheduled processes) performed since the last call to this function.<br />
%      Using this method and {@link reductions_total/0} at the same time will cause weird/wrong results.
-spec reductions_last_call() -> pos_integer().
reductions_last_call() ->
  {_Total, LastCall} = erlang:statistics(reductions),
  LastCall.

% @doc Returns the number of reductions executed by the process.<br />
%      If `Pid' is an `atom()', it will assume that it's the name of a registered process.
-spec reductions_process(atom() | pid()) -> pos_integer().
reductions_process(Pid) ->
  process_info_field(Pid, reductions).

% @doc Returns the total number of reductions (minus reductions performed in current time slices
%      of currently scheduled processes) performed at he local node.<br />
%      Using this method and {@link reductions_last_call/0} at the same time will cause weird/wrong results.
-spec reductions_total() -> pos_integer().
reductions_total() ->
  {Total, _LastCall} = erlang:statistics(reductions),
  Total.

% @doc Returns the numer of registered processes.
-spec registered_processes() -> pos_integer().
registered_processes() ->
  length(registered()).

% @doc Returns the total length of the run queues, that is, the number of processes that are ready to run on all available run queues.
-spec run_queue() -> non_neg_integer().
run_queue() ->
  erlang:statistics(run_queue).

% @doc Returns the number of packets received by `Socket'.
-spec socket_recv_package_count(inet:socket()) -> non_neg_integer().
socket_recv_package_count(Socket) ->
  socket_statistics(Socket, recv_cnt).

% @doc Returns the number of bytes received by `Socket'.
-spec socket_recv_size(inet:socket()) -> non_neg_integer().
socket_recv_size(Socket) ->
  socket_statistics(Socket, recv_oct).

% @doc Returns the number of packets sent by `Socket'.
-spec socket_send_package_count(inet:socket()) -> non_neg_integer().
socket_send_package_count(Socket) ->
  socket_statistics(Socket, send_cnt).

% @doc Returns the number of bytes sent from `Socket'.
-spec socket_send_size(inet:socket()) -> non_neg_integer().
socket_send_size(Socket) ->
  socket_statistics(Socket, send_oct).

% @doc Return the stack size of the process in words.<br />
%      If `Pid' is an `atom()', it will assume that it's the name of a registered process.
-spec stack_size(atom() | pid()) -> pos_integer().
stack_size(Pid) ->
  process_info_field(Pid, stack_size).

% Private

-spec process_info_field(pid() | atom(), atom()) -> non_neg_integer().
process_info_field(Pid, Field) when is_atom(Pid), Pid =/= undefined ->
  Pid2 = whereis(Pid),
  process_info_field(Pid2, Field);
process_info_field(Pid, Field) when is_pid(Pid) ->
  case process_info(Pid, Field) of
    {Field, Num} when is_number(Num) -> Num;
    {Field, List} when is_list(List) -> length(List);
    _ -> 0
  end.

-spec socket_statistics(inet:socket(), atom()) -> non_neg_integer().
socket_statistics(Socket, Key) ->
  case inet:getstat(Socket, [Key]) of
    {ok, Stats} -> proplists:get_value(Key, Stats, 0);
    {error, _Reason} -> 0
  end.

-spec r16_ets_limit() -> pos_integer().
r16_ets_limit() ->
  case os:getenv("ERL_MAX_ETS_TABLES") of
    false -> 1400;
    Limit -> list_to_integer(Limit)
  end.

% Tests (private functions)

-ifdef(TEST).
r16_ets_limit_test() ->
  ?assertEqual(1400, r16_ets_limit()),
  true = os:putenv("ERL_MAX_ETS_TABLES", "1600"),
  ?assertEqual(1600, r16_ets_limit()).
-endif.
