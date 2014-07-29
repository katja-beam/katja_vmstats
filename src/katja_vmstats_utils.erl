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
% @hidden
% @author Daniel Kempkens <daniel@kempkens.io>
% @copyright {@years} Daniel Kempkens
% @version {@version}
% @doc Internal utility of the Katja VM Stats application.

-module(katja_vmstats_utils).

% API
-export([
  parallel_map/2
]).

% API

-spec parallel_map(fun((A) -> B), [A]) -> [B].
parallel_map(_Fun, []) -> [];
parallel_map(Fun, List) ->
  Parent = self(),
  Worker = lists:map(fun(Item) ->
    spawn(fun() -> parallel_apply(Parent, Fun, Item) end)
  end, List),
  parallel_gather(Worker, []).

% Private

-spec parallel_apply(pid(), fun((A) -> B), A) -> B.
parallel_apply(Parent, Fun, Item) ->
  Worker = self(),
  Value = (catch Fun(Item)),
  Parent ! {Worker, Value}.

-spec parallel_gather([pid()], list()) -> list().
parallel_gather([], Acc) -> lists:reverse(Acc);
parallel_gather([Pid|Rest], Acc) ->
  receive
    {Pid, Value} -> parallel_gather(Rest, [Value | Acc])
  end.
