-module(re_test).
-export([test/0]).

pairs() ->
	[
		% empty
		{"", ""},
		{"asdas", ""},
		{"w", ""},
		{"", "a+"},
		{"", "a?"},
		{"", "a*"},

		% substrings
		{"ab", "ab"},
		{"ab", "abc"},
		{"abc", "ab"},
		{"wsaab", "ab"},
		{"wsaab", "sa"},

		% parentheses

		{"d", "((d))"},
		{"w", "((d))"},
		{"d", "(d)"},
		{"dd", "(d)(d)"},

		% repetition
		{"aaaaa", "a?"},
		{"", "a?"},
		{"a", "a?"},
		{"b", "a?"},
		{"vaaaa", "a?"},
		{"aaaaaaa", "aaaa?a?a?a?a?a?a?"},

		{"aaaaa", "a+b"},
		{"aaaaab", "a+b"},
		{"b", "a+b"},
		{"ab", "a+b"},
		{"bbbb", "a+b"},
		{"baa", "a+b"},
		{"aadd", "a+b"},
		{"aasb", "a+b"},
		{"a", "ab+"},
		{"", "ab+"},
		{"ab", "ab+"},
		{"bb", "ab+"},
		{"abb", "ab+"},

		{"aaaaa", "a*b"},
		{"aaaaab", "a*b"},
		{"b", "a*b"},
		{"ab", "a*b"},
		{"bbbb", "a*b"},
		{"baa", "a*b"},
		{"aadd", "a*b"},
		{"aasb", "a*b"},
		{"a", "ab*"},
		{"", "ab*"},
		{"ab", "ab*"},
		{"bb", "ab*"},
		{"abb", "ab*"},

		% concatenation
		{"abb", "a(b)"},
		{"ab", "a(b)"},
		{"ba", "a(b)"},
		{"ab", "(a)(b)"},
		{"ab", "(a)b(b)"},
		{"abb", "(a)b(b)"},

		% alternation
		{"cd", "a|e"},
		{"a", "a|e"},
		{"e", "a|e"},
		{"ae", "a|e"},
		{"as", "a|e"},
		{"se", "a|e"},
		{"cd", "(a)|e"},
		{"a", "(a)|e"},
		{"e", "(a)|e"},
		{"ae", "(a)|e"},
		{"as", "(a)|e"},
		{"se", "(a)|e"},
		{"a", "(a|b)|(c | (d|e))"},
		{"b", "(a|b)|(c | (d|e))"},
		{"c", "(a|b)|(c | (d|e))"},
		{"d", "(a|b)|(c | (d|e))"},
		{"e", "(a|b)|(c | (d|e))"},
		{"ab", "(a|b)|(c | (d|e))"},
		{"f", "(a|b)|(c | (d|e))"},
		{"ed", "(a|b)|(c | (d|e))"},
		{"ac", "(a|b)|(c | (d|e))"},

		% combined
		{"cde", "ab|(cd)?e"},
		{"abc", "ab|(cd)?e"},
		{"cdcd", "ab|(cd)?e"},
		{"bc", "a*b?(a|bc)"},
		{"aa", "a*b?(a|bc)"},
		{"ba", "a*b?(a|bc)"},

		{"0d0", "(1|2|3|4|5|6|7|8|9|0)+d(1|2|3|4|5|6|7|8|9|0)+"},
		{"1234512313111232d000000", "(1|2|3|4|5|6|7|8|9|0)+d(1|2|3|4|5|6|7|8|9|0)+"},
		{"d23423432", "(1|2|3|4|5|6|7|8|9|0)+d(1|2|3|4|5|6|7|8|9|0)+"},
		{"0d32d21312312", "(1|2|3|4|5|6|7|8|9|0)+d(1|2|3|4|5|6|7|8|9|0)+"}

	].

test() ->
	lists:map(fun({Str, RegEx}) ->
			io:format("--------------------------------------~n"),
			io:format("~s   ~s~n", [Str, RegEx]),
			ReResult = re_matches_str(Str, RegEx),
			MyResult = matches_str(Str, RegEx),
			io:format("re: ~p~n", [ReResult]),
			io:format("my: ~p~n", [MyResult]),
			true = matches_str(Str, RegEx) == re_matches_str(Str, RegEx)
		end,
		pairs()),
	ok.

re_matches_str(Str, RegEx) ->
	{ok, MP} = re:compile(RegEx),
	case (re:run(Str, MP)) of 
		{match, _} -> true;
		nomatch -> false
	end.

matches_str(Str, RegEx) ->
	re_processor:run(Str, re_compiler:compile(RegEx)).