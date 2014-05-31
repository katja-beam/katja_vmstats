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
% @doc The `katja_vmstats_metrics' module implements all available metrics.

-module(katja_vmstats_metrics).

% API
-export([
  error_logger_message_queue/0,
  loaded_modules/0,
  memory_atoms/0,
  memory_binaries/0,
  memory_ets/0,
  memory_processes/0,
  memory_system/0,
  memory_total/0,
  port_count/0,
  port_limit/0,
  port_utilization/0,
  process_count/0,
  process_limit/0,
  process_utilization/0,
  run_queue/0
]).

% API

% @doc Returns the size of the `error_logger' message queue at the local node.
-spec error_logger_message_queue() -> non_neg_integer().
error_logger_message_queue() ->
  ErrorLogger = whereis(error_logger),
  {message_queue_len, Size} = process_info(ErrorLogger, message_queue_len),
  Size.

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

% @doc Returns the total length of the run queues, that is, the number of processes that are ready to run on all available run queues.
-spec run_queue() -> non_neg_integer().
run_queue() ->
  erlang:statistics(run_queue).
