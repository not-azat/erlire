-module(gen_rope_tests).
-import(gen_rope, [leaf_rope/2, get_cache/1, split/3, merge/3, flatten/2]).
-include_lib("eunit/include/eunit.hrl").

-behaviour(gen_rope).
-export([length/2, cache/2, merge_values/3, split_value/3, merge_caches/3]).

-type cache() :: cache.
-type value() :: binary().


%%% rope callbacks %%%

-spec length(value(), term()) -> number().

length(BinString, _) -> byte_size(BinString).


-spec cache(value(), term()) -> cache().

cache(_, _) -> cache.


% length(Value1) + length(Value2) = length(merge_values(Value1, Value2))
-spec merge_values(value(), value(), term()) -> value().

merge_values(BinString1, BinString2, _) ->
	<<BinString1/binary, BinString2/binary>>.


% if {Value1, Value2} = split_value(Value) 
% then length(Value1) + length(Value2) = length(Value)
-spec split_value(value(), number(), term()) -> {value(), value()}.

split_value(BinString, Len, _) ->
	<<BinString1:Len/binary, BinString2/binary>> = BinString,
	{BinString1, BinString2}.


-spec merge_caches(cache(), cache(), term()) -> cache().
% dummy cache
merge_caches(cache, cache, _) -> cache.


%%% Test %%%%

params() -> {?MODULE, 5, give_back}.


-spec binary2rope(binary()) -> gen_rope:rope().

binary2rope(Binary) ->
	binary2rope(Binary, leaf_rope(<<"">>, params())).

binary2rope(<<>>, AccRope) ->
	AccRope;
binary2rope(Binary, AccRope) ->
	{_, LeafLength, _} = params(),
	case byte_size(Binary) =< LeafLength of 
		true ->
			NewRope = leaf_rope(Binary, params()),
			merge(AccRope, NewRope, params());
		false ->
			<<LeftChunk:LeafLength/binary, RestBinary/binary>> = Binary,
			NewRope = leaf_rope(LeftChunk, params()),
			binary2rope(RestBinary, merge(AccRope, NewRope, params()))
	end.


merge_split_equality_test_() ->
	{_, Tests} = lists:foldl(
		fun(I, {AccBin, AccTests}) ->
			Tests = [
				test_single_merge_split(AccBin),
				test_single_right_to_left(AccBin)],
			Inc = list_to_binary(integer_to_list(I)),
			{<<AccBin/binary, Inc/binary>>, [Tests | AccTests]}
		end,
		{<<>>, []},
		lists:flatten(lists:duplicate(7, "1234567"))),
	Tests.


test_single_right_to_left(Binary) ->
	lists:map(
		fun(Pos) ->
			Rope = binary2rope(Binary),
			{Rope1, Rope2} = split(Rope, Pos, params()),
			Binary1 = case flatten(Rope1, params()) of 
				undefined -> <<>>;
				Val1 -> Val1
			end,
			Binary2 = case flatten(Rope2, params()) of 
				undefined -> <<>>;
				Val2 -> Val2
			end,
			Rope12 = merge(Rope1, Rope2, params()),
			Rope21 = merge(Rope2, Rope1, params()),
			Binary34 = case flatten(Rope12, params()) of 
				undefined -> <<>>;
				Val34 -> Val34
			end,
			Binary43 = case flatten(Rope21, params()) of 
				undefined -> <<>>;
				Val43 -> Val43
			end,

			Binary12 = <<Binary1/binary, Binary2/binary>>,
			Binary21 = <<Binary2/binary, Binary1/binary>>,

			[	?_assertEqual(Binary12, Binary),
				?_assertEqual(Binary12, Binary34),
				?_assertEqual(Binary43, Binary21)]
		end,
		lists:seq(0, byte_size(Binary) + 1)).


test_single_merge_split(Binary) ->
	lists:map(
		fun(Pos) ->
			Rope = binary2rope(Binary),
			{Rope1, Rope2} = split(Rope, Pos, params()),
			Rope3 = merge(Rope1, Rope2, params()),
			Binary2 = flatten(Rope3, params()),
			[	?_assertEqual(
					Binary, 
					Binary2),
				?_assertEqual(
					gen_rope:length(Rope), 
					gen_rope:length(Rope1) + gen_rope:length(Rope2)),
				?_assertEqual(
					gen_rope:length(Rope3), 
					gen_rope:length(Rope1) + gen_rope:length(Rope2)),
				?_assertEqual(
					byte_size(Binary), 
					gen_rope:length(Rope3))]
		end,
		lists:seq(0, byte_size(Binary) + 1)).


conversion_equality_test_() ->
	{_, Tests} = lists:foldl(
		fun(I, {AccBin, AccTests}) ->
			Test = test_single_conversion(AccBin),
			Inc = list_to_binary(integer_to_list(I)),
			{<<AccBin/binary, Inc/binary>>, [Test | AccTests]}
		end,
		{<<>>, []},
		lists:flatten(lists:duplicate(11, "1234567"))),
	Tests.


test_single_conversion(Binary) ->
	Rope = binary2rope(Binary),
	Binary2 = flatten(Rope, params()),
	?_assertEqual(Binary2, Binary).

