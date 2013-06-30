-module(bitap).
-compile(export_all).
-export([find/2, find/3]).

-define(sl1(X), ((X bsl 1) bor 1)).

-spec find(Pattern::binary(), Str::binary()) -> [number()].

find(Pattern, Str) ->
	find(Pattern, Str, 0).


-spec find(Pattern::binary(), Str::binary(), K::number()) -> [number()].

find(Pattern, Str, K) ->
	Masks = compile_masks(Pattern),
	lists:reverse(
		match(Str, Masks, size(Pattern) - 1, K)).


-spec compile_masks(Pattern::binary()) -> [{Letter::char(), Mask::binary()}].

compile_masks(<<>>) ->
	throw('bad argument');
compile_masks(Pattern) ->
	compile_masks(Pattern, 0, []).

compile_masks(<<>>, _, AccDict) ->
	AccDict;
compile_masks(<<Letter,Rest/binary>>, J, AccDict) ->
	M1 = case (lists:keyfind(Letter, 1, AccDict)) of 
		{Letter, M} -> M;
		false -> 0
	end,
	AccDict1 = lists:keystore(Letter, 1, AccDict, {Letter, M1 bor (1 bsl J)}),
	compile_masks(Rest, J + 1, AccDict1).


-spec match(Str::binary(), Masks::dict(), N::number(), K::number()) 
		-> [{Pos::number(), Len::number(), EditDistance::number()}].

match(<<>>, _, _, _) ->
	[];
match(Str, Masks, N, K) ->
	match0(Str, Masks, 0, [ 0 || _ <- lists:seq(0, K) ], N, K, []).


% K = 0 (zero distance between substr and pattern)
match0(<<>>, _Masks, _J, _Rls, _N, _KMax, ResultsAcc) ->
	io:format("match0:~n\tStr: ~p~n\tMasks: ~p~n\tJ: ~p~n\tRls: ~p~n\tN: ~p~n\tKMax: ~p~n\tResultsAcc: ~p~n",
		[<<>>, _Masks, _J, _Rls, _N, _KMax, ResultsAcc]),
	ResultsAcc;
match0(<<Letter, Rest/binary>>, Masks, J, [Rl|Rls], N, KMax, ResultsAcc) ->
	io:format("match0:~n\tStr: ~p~n\tMasks: ~p~n\tJ: ~p~n\tRls: ~p~n\tN: ~p~n\tKMax: ~p~n\tResultsAcc: ~p~n",
		[<<Letter, Rest/binary>>, Masks, J, [Rl|Rls], N, KMax, ResultsAcc]),
	M = get_mask(Letter, Masks),
	io:format("\tnew M: ~p~n", [M]),
	Rc = ?sl1(Rl) band M,
	io:format("\tnew Rc: ~p~n", [Rc]),
	NMask = 1 bsl N, % TODO move to params
	io:format("\tnew Mask: ~p~n", [NMask]),
	ResultsAcc1 = case (Rc band NMask) of 
		NMask -> [{J-N, N+1, 0} | ResultsAcc];
		_ -> ResultsAcc
	end,
	matchK(<<Letter, Rest/binary>>, Masks, J, 1, [Rl|Rls], [Rc], N, KMax, ResultsAcc1).


matchK(<<>>, _Masks, _J, _K, _Rls, _Rcs, _N, _KMax, ResultsAcc) ->
	io:format("matchK:~n\tStr: ~p~n\tMasks: ~p~n\tJ: ~p~n\tK: ~p~n\tRls: ~p~n\tRcs: ~p~n\tN: ~p~n\tKMax: ~p~n\tResultsAcc: ~p~n",
		[<<>>, _Masks, _J, _K, _Rls, _Rcs, _N, _KMax, ResultsAcc]),
	ResultsAcc;
matchK(<<Letter, Rest/binary>>, Masks, J, K, _Rls, Rcs, N, KMax, ResultsAcc) when K > KMax ->
	io:format("matchK:~n\tStr: ~p~n\tMasks: ~p~n\tJ: ~p~n\tK: ~p~n\tRls: ~p~n\tRcs: ~p~n\tN: ~p~n\tKMax: ~p~n\tResultsAcc: ~p~n",
		[<<Letter, Rest/binary>>, Masks, J, K, _Rls, Rcs, N, KMax, ResultsAcc]),
	match0(Rest, Masks, J + 1, lists:reverse(Rcs), N, KMax, ResultsAcc);
matchK(<<Letter, Rest/binary>>, Masks, J, K, [Rl0, Rl1 | Rls], [Rc1 | Rcs], N, KMax, ResultsAcc) ->	
	io:format("matchK:~n\tStr: ~p~n\tMasks: ~p~n\tJ: ~p~n\tK: ~p~n\tRls: ~p~n\tRcs: ~p~n\tN: ~p~n\tKMax: ~p~n\tResultsAcc: ~p~n",
		[<<Letter, Rest/binary>>, Masks, J, K, [Rl0, Rl1 | Rls], [Rc1 | Rcs], N, KMax, ResultsAcc]),
	M = get_mask(Letter, Masks),
	Rc = ?sl1(Rl1) band M bor ?sl1(Rl0 bor Rc1) bor Rl0,
	NMask = 1 bsl N, % TODO move to params
	ResultsAcc1 = case (Rc band NMask) of 
		NMask -> [{J-N, N+1, K} | ResultsAcc];
		_ -> ResultsAcc
	end,
	matchK(<<Letter, Rest/binary>>, Masks, J, K+1, [Rl1 | Rls], [Rc, Rc1 | Rcs], N, KMax, ResultsAcc1).


get_mask(Letter, Masks) ->
	case (lists:keyfind(Letter, 1, Masks)) of 
		{Letter, Mask} -> Mask;
		false -> 0
	end.



