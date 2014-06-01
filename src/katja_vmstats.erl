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
% @doc This is the main module of the Katja VM Stats application: It provides the public API.<br />
%      While it is possible to use `katja_vmstats_collector' directly, the recommended way is to use the functions defined
%      in this module instead.

-module(katja_vmstats).

-type metric() :: atom() | {iolist(), atom(), [any()]} | {iolist(), module(), atom(), [any()]}.

-export_type([
  metric/0
]).

% API
-export([
  collect/1
]).

% API

% @doc Collects the specified metrics and sends them to Riemann. Delegates to {@link katja_vmstats_collector:collect/1}.
-spec collect(metric() | [metric()]) -> ok.
collect(Metrics) ->
  katja_vmstats_collector:collect(Metrics).
