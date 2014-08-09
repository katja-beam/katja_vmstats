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

### Working with timers

```erlang
AllTimers = katja_vmstats:get_timer(all), % => [{config, 1000}]
ConfigTimers = katja_vmstats:get_timer(config). % => [{config, 1000}]
```

You can get a list of registered timers and their intervals using `katja_vmstats:get_timer/1`. If a timer has more than one interval, it will show up multiple times in the returned list.

The timers that are specified via configuration options will always be registered under the name `config`. You can get all intervals by setting the name to `all`.

```erlang
Metrics = [{"reductions_process", reductions_process, [katja_vmstats_collector]}],
ok = katja_vmstats:start_timer(demo, [{interval, 1000}, {metrics, Metrics}]),
AllTimers = katja_vmstats:get_timer(all), % => [{demo, 1000}, {config, 1000}]
DemoTimers = katja_vmstats:get_timer(demo). % => [{demo, 1000}]
```

You can start new timers using `katja_vmstats:start_timer/2`. The first argument is the name of the timer and the second argument is a property list (or a list of property lists) in the format of the `collector` configuration option.

```erlang
ok = katja_vmstats:stop_timer(demo),
AllTimers = katja_vmstats:get_timer(all). % => [{config, 1000}]
```

Timers can be stopped using `katja_vmstats:stop_timer/1`. You can set the name to `all` in order to stop all registered timers.

## Resources

* [Generated EDoc](http://katja_vmstats.nifoc.pw/0.5/) ([All Versions](http://katja_vmstats.nifoc.pw))

## Related Projects

* [Katja](https://github.com/nifoc/katja) - A simple Riemann client written in Erlang

## License

[ISC](https://en.wikipedia.org/wiki/ISC_license).

```
Copyright (c) 2014, Daniel Kempkens <daniel@kempkens.io>

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
