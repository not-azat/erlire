-module(ire_test).
-export([test/0]).




% -export([new/3, merge/2, split/2, matches/1, length/1]).
% -export([convert_automata/1, find_transition/2, compose_transitions/2, matches/2]).

test() ->
	all_pairs_should_match().


test_regex(BinString, BinRegExStr) ->
	io:format("Should match: ~p  ~p~n", [BinString, BinRegExStr]),
	true = re_matches_str(BinString, BinRegExStr) == matches(BinRegExStr, BinString),
	ok.

matches(BinRegExStr, BinString) ->
	Automata = re_compiler:compile(BinRegExStr),
	TransitionAutomata = re_transition:convert_automata(Automata),
	Ire = ire:new(BinString, TransitionAutomata, 5),
	% io:format("Ire: ~n~p~n", [Ire]),
	ire:matches(Ire).

all_pairs_should_match() ->
	lists:foreach(
		fun({BinStr, BinRegExStr}) ->
			test_regex(BinStr, BinRegExStr)
		end,
		pairs()).

re_matches_str(Str, RegEx) ->
	{ok, MP} = re:compile(RegEx),
	case (re:run(Str, MP)) of 
		{match, _} -> true;
		nomatch -> false
	end.

pairs() ->
	[
		% not working yet...

		% % empty
		% {<<"">>, <<"">>},
		% {<<"asdas">>, <<"">>},
		% {<<"w">>, <<"">>},
		% {<<"">>, <<"a+">>},
		% {<<"">>, <<"a?">>},
		% {<<"">>, <<"a*">>},

		% substrings
		{<<"ab">>, <<"ab">>},
		{<<"ab">>, <<"abc">>},

		% parentheses

		{<<"d">>, <<"((d))">>},
		{<<"w">>, <<"((d))">>},
		{<<"d">>, <<"(d)">>},
		{<<"dd">>, <<"(d)(d)">>},

		% repetition
		{<<"a">>, <<"a?">>},
		{<<"aaaaaaa">>, <<"aaaa?a?a?a?a?a?a?">>},

		{<<"aaaaab">>, <<"a+b">>},
		{<<"ab">>, <<"a+b">>},
		{<<"bbbb">>, <<"a+b">>},
		{<<"baa">>, <<"a+b">>},
		{<<"aadd">>, <<"a+b">>},
		{<<"aasb">>, <<"a+b">>},
		{<<"a">>, <<"ab+">>},
		{<<"">>, <<"ab+">>},
		{<<"ab">>, <<"ab+">>},
		{<<"bb">>, <<"ab+">>},
		{<<"abb">>, <<"ab+">>},

		{<<"aaaaa">>, <<"a*b">>},
		{<<"aaaaab">>, <<"a*b">>},
		{<<"b">>, <<"a*b">>},
		{<<"ab">>, <<"a*b">>},
		{<<"aadd">>, <<"a*b">>},
		{<<"aasb">>, <<"a*sb">>},
		{<<"a">>, <<"ab*">>},
		{<<"">>, <<"ab*">>},
		{<<"ab">>, <<"ab*">>},
		{<<"bb">>, <<"ab*">>},
		{<<"abb">>, <<"ab*">>},

		% concatenation
		{<<"ab">>, <<"a(b)">>},
		{<<"ba">>, <<"a(b)">>},
		{<<"ab">>, <<"(a)(b)">>},
		{<<"ab">>, <<"(a)b(b)">>},
		{<<"abb">>, <<"(a)b(b)">>},

		% alternation
		{<<"cd">>, <<"a|e">>},
		{<<"a">>, <<"a|e">>},
		{<<"e">>, <<"a|e">>},
		{<<"cd">>, <<"(a)|e">>},
		{<<"a">>, <<"(a)|e">>},
		{<<"e">>, <<"(a)|e">>},
		{<<"ae">>, <<"(a)e|e">>},
		{<<"a">>, <<"(a|b)|(c|(d|e))">>},
		{<<"e">>, <<"(a|b)|(c|(d|e))">>},

		% combined
		{<<"cde">>, <<"ab|(cd)?e">>},
		{<<"ab">>, <<"ab|(cd)?e">>},
		{<<"cdcd">>, <<"ab|(cd)?e">>},
		{<<"bc">>, <<"a*b?(a|bc)">>},
		{<<"aa">>, <<"a*b?(a|bc)">>},
		{<<"ba">>, <<"a*b?(a|bc)">>}

	].