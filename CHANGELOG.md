# Changelog

## 0.7

* Update Katja dependency to [v0.8](https://github.com/nifoc/katja/tree/v0.8)
* New metrics: `ets_size/1`, `ets_size_total/0`, `socket_recv_package_count/1`, `socket_recv_size/1`, `socket_send_package_count/1`, `socket_send_size/1`

## 0.6

[Documentation](http://katja_vmstats.nifoc.pw/0.6/)

* Update Katja dependency to [v0.7](https://github.com/nifoc/katja/tree/v0.7)
* Update noesis dependency to [v0.2.1](https://github.com/nifoc/noesis/tree/v0.2.1)
* Allow setting a sample rate for asynchronous events using the `async_sample_rate` configuration option
* The following functions in the `katja_vmstats` module have to been removed: `get_timer/1`, `start_timer/2` and `stop_timer/1`
* The following functions have been added to the `katja_vmstats` module: `get_collection/1`, `start_collection/2`, `stop_collection/1`
* `send_async` and `async_sample_rate` can be set on a per-collection basis

## 0.5

[Documentation](http://katja_vmstats.nifoc.pw/0.5/)

* Switch from [Rebar](https://github.com/rebar/rebar) to [erlang.mk](https://github.com/ninenines/erlang.mk)
* Update Katja dependency to [v0.6](https://github.com/nifoc/katja/tree/v0.6)
* Add [noesis](https://github.com/nifoc/noesis/tree/v0.2) dependency
* Add `katja_vmstats:stop/0` to stop the Katja VM Stats application and all of its dependencies
* Allow setting a specific message transport (`config`, `detect`, `udp`, `tcp`) just for Katja VM Stats
* Allow sending events to Riemann asynchronously using the `send_async` configuration option
* Metric collections are now collected in parallel
* New metric: `registered_processes/0`

## 0.4

[Documentation](http://katja_vmstats.nifoc.pw/0.4/)

* Update Katja dependency to [v0.5](https://github.com/nifoc/katja/tree/v0.5)
* Add `katja_vmstats:get_timer/1`, `katja_vmstats:start_timer/2` and `katja_vmstats:stop_timer/1` to programmatically interact with timers/intervals

## 0.3

[Documentation](http://katja_vmstats.nifoc.pw/0.3/)

* Update Katja dependency to [v0.4](https://github.com/nifoc/katja/tree/v0.4)
* Add `katja_vmstats:start/0` to start the Katja VM Stats application and all of its dependencies
* New metrics: `heap_size/1`, `reductions_process/1`, `stack_size/1`

## 0.2

[Documentation](http://katja_vmstats.nifoc.pw/0.2/)

* New metrics: `all_message_queues/0`, `context_switches/0`, `exact_reductions_last_call/0`, `exact_reductions_total/0`, `garbage_collection_runs/0`, `garbage_collection_words_reclaimed/0`, `io_in/0`, `io_out/0`, `links/1`, `memory_process/1`, `message_queue/1`, `monitors/1`, `reductions_last_call/0`, `reductions_total/0`
* Arbitrary functions (that return `number()`) can be used for metric collection (see: `katja_vmstats:metric()` and README)
* Add the `delay_collection` configuration option, which allows the initial metrics collection to be delayed by the specified amount of milliseconds

## 0.1

[Documentation](http://katja_vmstats.nifoc.pw/0.1/)

* Initial release
