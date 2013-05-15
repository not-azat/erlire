-module(rope_test).
-export([test/0]).

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
			Rope = rope:binary2rope(Binary),
			{Rope1, Rope2} = rope:split(Rope, Pos),
			Binary1 = rope:rope2binary(Rope1),
			Binary2 = rope:rope2binary(Rope2),
			Rope12 = rope:merge(Rope1, Rope2),
			Rope21 = rope:merge(Rope2, Rope1),
			Binary34 = rope:rope2binary(Rope12),
			Binary43 = rope:rope2binary(Rope21),

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
			Rope = rope:binary2rope(Binary),
			{Rope1, Rope2} = rope:split(Rope, Pos),
			Rope3 = rope:merge(Rope1, Rope2),
			Binary2 = rope:rope2binary(Rope3),
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
	Rope = rope:binary2rope(Binary),
	Binary2 = rope:rope2binary(Rope),
	Binary2 == Binary.






to_list(X) ->
	lists:flatten(io_lib:format("~p", [X])). 


maybe_print(true, Str) ->
	io:format("~s~n", [Str]);
maybe_print(_, _) ->
	ok. 