-module(ire_tests).
-import(ire, [
	new/3, merge/2, split/2, matches/1, % interface
	length/2, cache/2, merge_values/3, split_value/3, merge_caches/3]). % behaviour
-include_lib("eunit/include/eunit.hrl").

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
		eq(<<"">>, <<"ab+">>),
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
		eq(<<"">>, <<"ab*">>),
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
		eq(<<"ba">>, <<"a*b?(a|bc)">>)
	].

eq(BinStr, BinRegExStr) ->
	?_assertEqual(
				re_matches(BinStr, BinRegExStr), 
				matches(BinStr, BinRegExStr)).

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
