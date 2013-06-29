-module(bitap).
-compile(export_all).
-export([find/2]).

-spec find(Pattern::binary(), Bitstring::binary()) -> [number()].

find(Pattern, Bitstring) ->
	Masks = compile_masks(Pattern),
	lists:reverse(
		match(Bitstring, Masks, size(Pattern) - 1)).


-spec compile_masks(Pattern::binary()) -> dict().
%% dict(Letter => Mask::binary()).
compile_masks(<<>>) ->
	throw('bad argument');
compile_masks(Pattern) ->
	compile_masks(Pattern, 0, dict:new()).

compile_masks(<<>>, _, AccDict) ->
	AccDict;
compile_masks(<<Letter,Rest/binary>>, I, AccDict) ->
	M1 = case (dict:find(Letter, AccDict)) of 
		{ok, M} -> M;
		error -> 0
	end,
	AccDict1 = dict:store(Letter, M1 bor (1 bsl I), AccDict),
	compile_masks(Rest, I + 1, AccDict1).


-spec match(Bitstring::binary(), Masks::dict(), N::number()) -> [{Pos::number(), Len::number()}].

match(Bitstring, Masks, N) ->
	match(Bitstring, Masks, 0, 0, N, []).

match(<<>>, _Masks, _I, _R, _N, ResultsAcc) ->
	ResultsAcc;
match(<<Letter, Rest/binary>>, Masks, I, R, N, ResultsAcc) ->
	M1 = case (dict:find(Letter, Masks)) of 
		{ok, M} -> M;
		error -> 0
	end,
	R1 = ((R bsl 1) bor 1) band M1,
	NMask = 1 bsl N,
	case (R1 band NMask) of 
		NMask	-> match(Rest, Masks, I + 1, R1, N, [{I - N, N + 1} | ResultsAcc]);
		_ 		-> match(Rest, Masks, I + 1, R1, N, ResultsAcc)
	end.






