-module(bitset).

-export([new/1, from_binary/1, from_binary/2, to_binary/1, is_empty/1, each_bit/2, set/2, is_set/2]).
-export([bit_andnot/2, bit_or/2, bit_and/2, to_ordset/1]).

new(Size) ->
    {bitset, Size, []}.

to_binary({bitset, Size, Set}) ->
    PadBits = case (Size rem 8) == 0 of
                  true  -> 0;
                  false -> 8-(Size rem 8)
              end,
    << <<Bit:1>> || Bit <- [ case ordsets:is_element(I, Set) of
                               true -> 1; false -> 0
                             end || I <- lists:seq(0, Size+PadBits-1)] >>.

to_ordset({bitset, _Size, Set}) ->
  Set.

from_binary(Binary) ->
  from_binary(Binary, bit_size(Binary)).

from_binary(Binary, Size) ->
    from_binary(Binary, Size, bit_size(Binary)).

from_binary(Binary, Size, BitSize) when Size =< BitSize ->
    <<BitString:Size/bitstring, _/bitstring>> = Binary,
    OnesAndZeros = [ Bit || <<Bit:1>> <= BitString],
    {bitset, Size, [ N || {Bit,N} <- lists:zip(OnesAndZeros, lists:seq(0, Size-1)), Bit == 1 ]}.

is_empty({bitset, _Size, []}) -> true;
is_empty({bitset, _Size, _}) -> false.

each_bit(Fun, {bitset, _Size, Set}) -> lists:foreach(Fun, Set).

set({bitset, Size, Set}, Bit) ->
    {bitset, max(Size,Bit), ordsets:add_element(Bit, Set)}.

is_set({bitset, _Size, Set}, Bit) -> ordsets:is_element(Bit, Set).

bit_and({bitset, Size1, Set1}, {bitset, Size2, Set2}) ->
    {bitset, min(Size1,Size2), ordsets:intersection(Set1, Set2)}.

bit_andnot({bitset, Size1, Set1}, {bitset, Size2, Set2}) ->
    {bitset, min(Size1,Size2), ordsets:subtract(Set1, Set2)}.

bit_or({bitset, Size1, Set1}, {bitset, Size2, Set2}) ->
    {bitset, max(Size1,Size2), ordsets:union(Set1, Set2)}.


