% Copyright (c) 2014, Daniel Kempkens <daniel@kempkens.io>
%
% Permission to use, copy, modify, and/or distribute this software for any purpose with or without fee is hereby granted,
% provided that the above copyright notice and this permission notice appear in all copies.
%
% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
% WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
% DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
% NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(katja_vmstats_utils_test).

-include_lib("eunit/include/eunit.hrl").

current_timestamp_test() ->
  ?assert(katja_vmstats_utils:current_timestamp() > 0),
  TimeA = katja_vmstats_utils:current_timestamp(),
  ok = timer:sleep(1100),
  TimeB = katja_vmstats_utils:current_timestamp(),
  ?assert(TimeA < TimeB).

parallel_map_test() ->
  FunA = fun(X) -> X + 1 end,
  ListA = [1, 2, 10, 66, 99, 6, 3, 9000],
  ?assertEqual(lists:map(FunA, ListA), katja_vmstats_utils:parallel_map(FunA, ListA)),
  FunB = fun({_K, V}) -> V end,
  ListB = [{x, <<"hello">>}, {y, <<"test">>}, {z, <<"foo">>}],
  ?assertEqual(lists:map(FunB, ListB), katja_vmstats_utils:parallel_map(FunB, ListB)).
