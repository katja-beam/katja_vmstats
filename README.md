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

**service**: Base value of the `service` field of events

**collector**: A list of metrics that will be collected in the given interval(s)

## Examples

In general, you don't have to do anything (apart from maybe changing the default configuration) to make `katja_vmstats` work.

A list of all available, collectable metrics can be found in the `katja_vmstats_metrics` module. Every exported function defined in that module can be collected.

### Collecting metrics manually

```erlang
ok = katja_vmstats:collect(ets_count),
ok = katja_vmstats:collect([ets_limit, ets_utilization]).
```

`katja_vmstats:collect/1` takes a single atom or a list of atoms, which have to map to functions defined and exported in the `katja_vmstats_metrics` module. Every time this function is called, the specified metrics will be collected.

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
