-module(bitap_bench).
-export([run/1, find_bitap/3, find_binary/3, find_dummy/3]).

run(N) ->
	{ok, Bin} = file:read_file("test_data/text1.txt"),
	Pattern = <<"that">>,
	spawn(fun() -> io:format("find_bitap: ~p~n", [element(1, timer:tc(?MODULE, find_bitap, [Pattern, Bin, N]))]) end),
	spawn(fun() -> io:format("find_binary: ~p~n", [element(1, timer:tc(?MODULE, find_binary, [Pattern, Bin, N]))]) end),
	spawn(fun() -> io:format("find_dummy: ~p~n", [element(1, timer:tc(?MODULE, find_dummy, [Pattern, Bin, N]))]) end).



find_bitap(Pattern, Bin, N) ->
	[ bitap:find(Pattern, Bin) || _ <- lists:seq(1, N)],
	ok.

find_binary(Pattern, Bin, N) ->
	[ binary:matches(Bin, [Pattern]) || _ <- lists:seq(1, N) ],
	ok.

find_dummy(Pattern, Bin, N) ->
	[ find2(Pattern, Bin) || _ <- lists:seq(1, N)],
	ok.




find2(Pattern, Bitstring) ->
	Masks = compile_masks(Pattern),
	lists:reverse(
		match(Bitstring, Masks, size(Pattern) - 1)).

compile_masks(<<>>) ->
	throw('bad argument');
compile_masks(Pattern) ->
	compile_masks(Pattern, 0, []).

compile_masks(<<>>, _, AccDict) ->
	AccDict;
compile_masks(<<Letter,Rest/binary>>, I, AccDict) ->
	M1 = case (lists:keyfind(Letter, 1, AccDict)) of 
		{Letter, M} -> M;
		false -> 0
	end,
	AccDict1 = lists:keystore(Letter, 1, AccDict, {Letter, M1 bor (1 bsl I)}),
	compile_masks(Rest, I + 1, AccDict1).


match(Bitstring, Masks, N) ->
	match(Bitstring, Masks, 0, 0, N, []).

match(<<>>, _Masks, _I, _R, _N, ResultsAcc) ->
	ResultsAcc;
match(<<Letter, Rest/binary>>, Masks, I, R, N, ResultsAcc) ->
	M1 = case (lists:keyfind(Letter, 1, Masks)) of 
		{Letter, M} -> M;
		false -> 0
	end,
	R1 = ((R bsl 1) bor 1) band M1,
	NMask = 1 bsl N,
	case (R1 band NMask) of 
		NMask	-> match(Rest, Masks, I + 1, R1, N, [{I - N, N + 1} | ResultsAcc]);
		_ 		-> match(Rest, Masks, I + 1, R1, N, ResultsAcc)
	end.


