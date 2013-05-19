-module(rope_test).
-export([test/0]).

-behaviour(rope).
-export([length/1, cache/1, merge_values/2, split_value/2, merge_caches/2]).

-type cache() :: cache.
-type value() :: binary().


%%% rope callbacks %%%

-spec length(value()) -> number().

length(BinString) -> byte_size(BinString).


-spec cache(value()) -> cache().

cache(_) -> cache.


% length(Value1) + length(Value2) = length(merge_values(Value1, Value2))
-spec merge_values(value(), value()) -> value().

merge_values(BinString1, BinString2) ->
	<<BinString1/binary, BinString2/binary>>.


% if {Value1, Value2} = split_value(Value) 
% then length(Value1) + length(Value2) = length(Value)
-spec split_value(value(), number()) -> {value(), value()}.

split_value(BinString, Len) ->
	<<BinString1:Len/binary, BinString2/binary>> = BinString,
	{BinString1, BinString2}.


-spec merge_caches(cache(), cache()) -> cache().
% dummy cache
merge_caches(cache, cache) -> cache.


%%% Test %%%%

params() -> {?MODULE, 5}.


-spec binary2rope(binary()) -> rope:rope().

binary2rope(Binary) ->
	binary2rope(Binary, rope:leaf_rope(<<"">>, params())).

binary2rope(<<>>, AccRope) ->
	AccRope;
binary2rope(Binary, AccRope) ->
	{_, LeafLength} = params(),
	case byte_size(Binary) =< LeafLength of 
		true ->
			NewRope = rope:leaf_rope(Binary, params()),
			rope:merge(AccRope, NewRope, params());
		false ->
			<<LeftChunk:LeafLength/binary, RestBinary/binary>> = Binary,
			NewRope = rope:leaf_rope(LeftChunk, params()),
			binary2rope(RestBinary, rope:merge(AccRope, NewRope, params()))
	end.


test() ->
	io:format("Rope test begins~n"),
	test_conversion_equality(),
	test_merge_split_equality(),
	io:format("Rope test ends~n").


test_merge_split_equality() ->
	lists:foldl(
		fun(I, AccBin) ->
			test_single_merge_split(AccBin),
			test_single_right_to_left(AccBin),
			Inc = list_to_binary(integer_to_list(I)),
			<<AccBin/binary, Inc/binary>>
		end,
		<<>>,
		lists:flatten(lists:duplicate(17, "1234567"))).


test_single_right_to_left(Binary) ->
	lists:foreach(
		fun(Pos) ->
			Rope = binary2rope(Binary),
			{Rope1, Rope2} = rope:split(Rope, Pos, params()),
			Binary1 = case rope:flatten(Rope1, params()) of 
				undefined -> <<>>;
				Val1 -> Val1
			end,
			Binary2 = case rope:flatten(Rope2, params()) of 
				undefined -> <<>>;
				Val2 -> Val2
			end,
			Rope12 = rope:merge(Rope1, Rope2, params()),
			Rope21 = rope:merge(Rope2, Rope1, params()),
			Binary34 = case rope:flatten(Rope12, params()) of 
				undefined -> <<>>;
				Val34 -> Val34
			end,
			Binary43 = case rope:flatten(Rope21, params()) of 
				undefined -> <<>>;
				Val43 -> Val43
			end,

			Binary12 = <<Binary1/binary, Binary2/binary>>,
			Binary21 = <<Binary2/binary, Binary1/binary>>,

			Result1 = Binary12 == Binary,
			Result2 = Binary12 == Binary34,
			Result3 = Binary43 == Binary21,

			maybe_print(not (Result1 and Result2 and Result3), "fail cincat with:~n" ++ to_list(Binary) ++ "~n on pos: " ++ to_list(Pos))
		end,
		lists:seq(0, byte_size(Binary) + 1)).


test_single_merge_split(Binary) ->
	lists:foreach(
		fun(Pos) ->
			Rope = binary2rope(Binary),
			{Rope1, Rope2} = rope:split(Rope, Pos, params()),
			Rope3 = rope:merge(Rope1, Rope2, params()),
			Binary2 = rope:flatten(Rope3, params()),
			Result = Binary == Binary2,
			maybe_print(not Result, "fail with:~n" ++ to_list(Binary) ++ "~n on pos: " ++ to_list(Pos)),
			Result2 = rope:length(Rope) == (rope:length(Rope1) + rope:length(Rope2)),
			Result3 = rope:length(Rope3) == (rope:length(Rope1) + rope:length(Rope2)),
			Result4 = byte_size(Binary) == rope:length(Rope3),
			maybe_print(not (Result3 and Result2 and Result4), "fail length with:~n" ++ to_list(Binary) ++ "~n on pos: " ++ to_list(Pos))
		end,
		lists:seq(0, byte_size(Binary) + 1)).


test_conversion_equality() ->
	lists:foldl(
		fun(I, AccBin) ->
			Result = test_single_conversion(AccBin),
			maybe_print(not Result, "fail with:~n" ++ to_list(AccBin) ++ "~n of length: " ++ to_list(byte_size(AccBin))),
			Inc = list_to_binary(integer_to_list(I)),
			<<AccBin/binary, Inc/binary>>
		end,
		<<>>,
		lists:flatten(lists:duplicate(23, "1234567"))).


test_single_conversion(Binary) ->
	Rope = binary2rope(Binary),
	Binary2 = rope:flatten(Rope, params()),
	Binary2 == Binary.


to_list(X) ->
	lists:flatten(io_lib:format("~p", [X])). 


maybe_print(true, Str) ->
	io:format("~s~n", [Str]);
maybe_print(_, _) ->
	ok. 