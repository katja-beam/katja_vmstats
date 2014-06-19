# Katja VM Stats

Easily send information about the Erlang VM to Riemann.

[![Build Status](https://travis-ci.org/nifoc/katja_vmstats.png)](https://travis-ci.org/nifoc/katja_vmstats)

## Status

This is alpha software. Things might still change in ways that break everything.

## Configuration

```erlang
% Defaults
[
  {katja_vmstats, [
    {service, "katja_vmstats"},
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

## Resources

* [Generated EDoc](http://katja_vmstats.nifoc.pw/0.3/) ([All Versions](http://katja_vmstats.nifoc.pw))

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
