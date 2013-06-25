-module(ire_tests).
-import(ire, [
	new/3, merge/2, split/2, matches/1, % interface
	length/2, cache/2, merge_values/3, split_value/3, merge_caches/3]). % behaviour
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

all_pairs_as_re_test_() ->
	[
		% % empty
		% eq(<<"">>, <<"">>),
		% eq(<<"asdas">>, <<"">>),
		% eq(<<"w">>, <<"">>),
		% eq(<<"">>, <<"a+">>),
		% eq(<<"">>, <<"a?">>),
		% eq(<<"">>, <<"a*">>),

		% substrings
		eq(<<"ab">>, <<"ab">>),
		eq(<<"ab">>, <<"abc">>),

		% parentheses

		eq(<<"d">>, <<"((d))">>),
		eq(<<"w">>, <<"((d))">>),
		eq(<<"d">>, <<"(d)">>),
		eq(<<"dd">>, <<"(d)(d)">>),

		% repetition
		eq(<<"a">>, <<"a?">>),
		eq(<<"aaaaaaa">>, <<"aaaa?a?a?a?a?a?a?">>),

		eq(<<"aaaaab">>, <<"a+b">>),
		eq(<<"ab">>, <<"a+b">>),
		eq(<<"bbbb">>, <<"a+b">>),
		eq(<<"baa">>, <<"a+b">>),
		eq(<<"aadd">>, <<"a+b">>),
		eq(<<"aasb">>, <<"a+b">>),
		eq(<<"a">>, <<"ab+">>),
		%eq(<<"">>, <<"ab+">>),
		eq(<<"ab">>, <<"ab+">>),
		eq(<<"bb">>, <<"ab+">>),
		eq(<<"abb">>, <<"ab+">>),

		eq(<<"aaaaa">>, <<"a*b">>),
		eq(<<"aaaaab">>, <<"a*b">>),
		eq(<<"b">>, <<"a*b">>),
		eq(<<"ab">>, <<"a*b">>),
		eq(<<"aadd">>, <<"a*b">>),
		eq(<<"aasb">>, <<"a*sb">>),
		eq(<<"a">>, <<"ab*">>),
		%eq(<<"">>, <<"ab*">>),
		eq(<<"ab">>, <<"ab*">>),
		eq(<<"bb">>, <<"ab*">>),
		eq(<<"abb">>, <<"ab*">>),

		% concatenation
		eq(<<"ab">>, <<"a(b)">>),
		eq(<<"ba">>, <<"a(b)">>),
		eq(<<"ab">>, <<"(a)(b)">>),
		eq(<<"ab">>, <<"(a)b(b)">>),
		eq(<<"abb">>, <<"(a)b(b)">>),

		% alternation
		eq(<<"cd">>, <<"a|e">>),
		eq(<<"a">>, <<"a|e">>),
		eq(<<"e">>, <<"a|e">>),
		eq(<<"cd">>, <<"(a)|e">>),
		eq(<<"a">>, <<"(a)|e">>),
		eq(<<"e">>, <<"(a)|e">>),
		eq(<<"ae">>, <<"(a)e|e">>),
		eq(<<"a">>, <<"(a|b)|(c|(d|e))">>),
		eq(<<"e">>, <<"(a|b)|(c|(d|e))">>),

		% combined
		eq(<<"cde">>, <<"ab|(cd)?e">>),
		eq(<<"ab">>, <<"ab|(cd)?e">>),
		eq(<<"cdcd">>, <<"ab|(cd)?e">>),
		eq(<<"bc">>, <<"a*b?(a|bc)">>),
		eq(<<"aa">>, <<"a*b?(a|bc)">>),
		eq(<<"ba">>, <<"a*b?(a|bc)">>),
		eq(<<"abc">>, <<"b">>)
	].

eq(BinStr, BinRegExStr) ->
	{<<BinStr/binary," ",BinRegExStr/binary>>,
		?_assertEqual(
				re_matches(BinStr, <<"^",BinRegExStr/binary,"$">>), 
				matches(BinStr, BinRegExStr))}.

matches(BinString, BinRegExStr) ->
	Automata = re_compiler:compile(BinRegExStr),
	TransitionAutomata = re_transition:convert_automata(Automata),
	Ire = new(BinString, TransitionAutomata, 5),
	matches(Ire).

re_matches(Str, RegEx) ->
	{ok, MP} = re:compile(RegEx),
	case (re:run(Str, MP)) of 
		{match, _} -> true;
		nomatch -> false
	end.

merge_test_() ->
	[
		m([<<"b">>, <<"b">>], <<"b">>),
		m([<<"abcababcababcab">>], <<"(a|bc|b)*">>),
		m([<<"abcabab">>, <<"cababcab">>], <<"(a|bc|b)*">>),
		m([<<"aaaaaaaaaa">>, <<"aaaaa">>, <<"aaaaaaaa">>, <<"aaaaa">>], <<"a?a?b?c?d?e?f?2?1?aaaaaaaaaaaa">>),
		m([<<"aaaa">>, <<"aaaaaa">>, <<"aaaaa">>, <<"aaaaaaaa">>, <<"aaaaa">>], <<"ab(c?)+|a*">>),
		m([<<"">>, <<"34532342">>, <<"525667454">>, <<"12313123">>, <<"c">>], <<"(0|1|2|3|4|5|6|7|8|9)c">>)
	].

m(Bins, RegEx) ->
	Automata = re_compiler:compile(RegEx),
	TransitionAutomata = re_transition:convert_automata(Automata),
	Ires = [ new(S, TransitionAutomata, 4) || S <- Bins ],
	Ire = lists:foldl(
		fun(I, AccI) ->
			merge(AccI, I)
		end,
		new(<<>>, TransitionAutomata, 4),
		Ires),
	BinStr = << <<S/binary>> || S <- Bins >>,
	{<<BinStr/binary," ",RegEx/binary>>, ?_assertEqual(
		ire:matches(Ire),
		re_matches(BinStr, <<"^", RegEx/binary, "$">>))}.

split_test_() ->
	[
		s(<<"aaaaaaaaaaaaaaaaaaaaaaaaa">>, [3, 5], <<"(a|bc|b)*">>),
		s(<<"aaaaabbbbbbcddddddeee">>, [3, 2, 5, 4], <<"a*b*c*d*e*f*">>),
		s(<<"123123545656455423425646c">>, [3, 1, 2, 3, 4, 5], <<"(0|1|2|3|4|5|6|7|8|9)c">>)
	].

s(Bin, Ns, RegEx) ->
	Automata = re_compiler:compile(RegEx),
	TransitionAutomata = re_transition:convert_automata(Automata),
	Ire = new(Bin, TransitionAutomata, 3),
	Ires = split_by(Ire, Ns, []),
	Ire2 = lists:foldl(
		fun(I, AccI) ->
			merge(AccI, I)
		end,
		new(<<>>, TransitionAutomata, 4),
		Ires),
	{<<Bin/binary," ",RegEx/binary>>, ?_assertEqual(
		ire:matches(Ire),
		ire:matches(Ire2))}.


split_by(Ire, [], Acc) -> 
	lists:reverse([Ire | Acc]);
split_by(Ire, [N | Rest], Acc) ->
	{Ire1, Ire2} = split(Ire, N),
	split_by(Ire2, Rest, [Ire1 | Acc]).











