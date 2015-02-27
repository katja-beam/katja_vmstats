# Katja VM Stats

Easily send information about the Erlang VM to Riemann.

[![Build Status](https://travis-ci.org/nifoc/katja_vmstats.png)](https://travis-ci.org/nifoc/katja_vmstats) [![Coverage Status](https://coveralls.io/repos/nifoc/katja_vmstats/badge.png?branch=master)](https://coveralls.io/r/nifoc/katja_vmstats?branch=master)

## Status

This is alpha software. Things might still change in ways that break everything.

## Configuration

```erlang
% Defaults
[
  {katja_vmstats, [
    {service, "katja_vmstats"},
    {transport, config},
    {send_async, false},
    {async_sample_rate, 1.0},
    {delay_collection, 0},
    {collector, [
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
    ]}
  ]}
].
```

**service**: Base value for the `service` field of events

**transport**: The message transport that should be used (supported: `config`, `detect`, `udp`, `tcp`)

**send_async**: Wether or not data should be send to Riemann asynchronously

**async_sample_rate**: Sample rate that will be used if messages are send asynchronously (useful if not *every* collection has to be send to Riemann all the time)

**delay_collection**: Delays the initial collection of metrics by the specified amount of milliseconds

**collector**: A list of metrics that will be collected in the given interval(s)

## Examples

In general, you don't have to do anything (apart from maybe changing the default configuration) to make `katja_vmstats` work.

A list of all available, collectable metrics can be found in the `katja_vmstats_metrics` module. Every exported function defined in that module can be collected.

### Collecting metrics manually

```erlang
ok = katja_vmstats:collect(ets_count),
ok = katja_vmstats:collect([ets_limit, ets_utilization]),
ok = katja_vmstats:collect({"katja_vmstats_collector_message_queue", message_queue, [katja_vmstats_collector]}),
ok = katja_vmstats:collect({"process_limit", erlang, system_info, [process_limit]}).
```

`katja_vmstats:collect/1` takes a single `katja_vmstats:metric()` or a list of `katja_vmstats:metric()`. The following "formats" are supported:

* `atom()`: A function defined and exported in the `katja_vmstats_metrics` module.
* `{iolist(), atom(), [any()]}`: A function defined and exported in the `katja_vmstats_metrics` module. The first tuple field is the name of the metric, the second and the third field are the *FA* part of *MFA*.
* `{iolist(), module(), atom(), [any()]}`: Any function in any module that returns a `number()`. The first tuple field is the name of the metric, the following fields are *MFA*.

### Working with collections

```erlang
AllCollections = katja_vmstats:get_collection(all), % => [[{name, config}, {metrics, […]}, {interval, 1000}, {send_async, false}, {async_sample_rate, 1.0}]]
ConfigCollection = katja_vmstats:get_collection(config). % => [[{name, config}, {metrics, […]}, {interval, 1000}, {send_async, false}, {async_sample_rate, 1.0}]]
```

You can get a list of registered collections using `katja_vmstats:get_collection/1`. If a collection consists of multiple `katja_vmstats:collection()` entries, it will show up multiple times in the returned list.

The collections that are specified via configuration options will always be registered under the name `config`. You can get all intervals by setting the name to `all`.

```erlang
Metrics = [{"reductions_process", reductions_process, [katja_vmstats_collector]}],
ok = katja_vmstats:start_collection(demo, [{interval, 1000}, {metrics, Metrics}]),
AllCollections = katja_vmstats:get_collection(all), % => [[…], […]]
DemoCollection = katja_vmstats:get_collection(demo). % => [[{name, demo}, {metrics, […]}, {interval, 1000}, {send_async, false}, {async_sample_rate, 1.0}]]
```

You can start new collections using `katja_vmstats:start_collection/2`. The first argument is the name of the collection and the second argument is a property list (or a list of property lists) in the format of `katja_vmstats:collection()`. The `name` attribute of the property list will be ignored.

```erlang
ok = katja_vmstats:stop_collection(demo),
AllCollections = katja_vmstats:get_collection(all). % => [[{name, config}, {metrics, […]}, {interval, 1000}, {send_async, false}, {async_sample_rate, 1.0}]]
```

Collections can be stopped using `katja_vmstats:stop_collection/1`. You can set the name to `all` in order to stop all registered collections.

## Resources

* [Generated EDoc](http://katja_vmstats.nifoc.pw/0.6/) ([All Versions](http://katja_vmstats.nifoc.pw))

## Related Projects

* [Katja](https://github.com/nifoc/katja) - A simple Riemann client written in Erlang

## License

[ISC](https://en.wikipedia.org/wiki/ISC_license).

```
Copyright (c) 2014-2015, Daniel Kempkens <daniel@kempkens.io>

Permission to use, copy, modify, and/or distribute this software for any purpose
with or without fee is hereby granted, provided that the above copyright notice
and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS
OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF
THIS SOFTWARE.
```
