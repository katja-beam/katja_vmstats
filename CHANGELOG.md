# Changelog

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
