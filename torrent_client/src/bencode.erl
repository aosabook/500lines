-module(bencode).

-ifdef(TEST).
-compile(export_all).
-endif.

-export([decode/1,encode/1,find/2,get/2]).

encode(Data) -> {ok, erlang:iolist_to_binary( encode2(Data) )}.
decode(Bin)  -> {Result, <<>>} = decode2(Bin),
                {ok, Result}.

find({PropList}, Key) when is_binary(Key) ->
    case lists:keyfind(Key, 1, PropList) of
        false -> false;
        {_, Value} -> {ok, Value}
    end;
find(Dict, Key) when is_atom(Key) ->
    find(Dict, atom_to_binary(Key, utf8)).

get(Dict, Key) -> {ok, Value} = find(Dict, Key), Value.

decode2(<<$i,I,Rest/binary>>) -> decode_int(I-$0, Rest);
decode2(<<$l,Rest/binary>>)   -> decode_list(Rest, []);
decode2(<<$d,Rest/binary>>)   -> decode_dictionary(Rest, []);
decode2(Bin)                  -> decode_string(0, Bin).

decode_int(Val, <<$e, Rest/binary>>) -> {Val, Rest};
decode_int(Val, <<N, Rest/binary>>)  -> decode_int(Val * 10 + N-$0, Rest).

decode_list(<<$e,Rest/binary>>, Acc) -> {lists:reverse(Acc), Rest};
decode_list(Bin, Acc)                -> {Elm, Rest} = decode2(Bin),
                                        decode_list(Rest, [Elm|Acc]).

decode_dictionary(<<$e,Rest/binary>>, Acc) ->
    {{lists:reverse(Acc)}, Rest};
decode_dictionary(Bin, Acc) ->
    {Key,   Rest1} = decode2(Bin),
    {Value, Rest2} = decode2(Rest1),
    decode_dictionary(Rest2, [{Key,Value}|Acc]).

decode_string(Len, <<$:, Rest/binary>>) -> <<String:Len/binary, Rest2/binary>> = Rest, {String, Rest2};
decode_string(Len, <<N, Rest/binary>>)  -> decode_string(Len*10 + (N-$0), Rest).

encode2(I) when is_integer(I) -> [$i, erlang:integer_to_binary(I, 10),$e];
encode2(S) when is_binary(S)  -> [erlang:integer_to_binary(byte_size(S)), $:, S];
encode2(L) when is_list(L)    -> [$l, lists:map(fun encode2/1, L), $e];
encode2({D}) when is_list(D)  -> [$d, [ [encode2(K), encode2(V)] || {K,V} <- D ], $e].

-ifdef(TEST).

bdecode_test() ->
    {ok, {[{<<"cow">>, <<"moo">>}, {<<"spam">>, <<"eggs">>}]}} = decode(<<"d3:cow3:moo4:spam4:eggse">>),
    {ok, [<<"spam">>, <<"eggs">>]} = decode(<<"l4:spam4:eggse">>).

bencode_test() ->
    {ok, <<"l4:spam4:eggse">>} = encode([<<"spam">>, <<"eggs">>]),
    ok.

-endif.
