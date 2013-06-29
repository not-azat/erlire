-module(bitap_tests).
-import(bitap, [find/2]).
-include_lib("eunit/include/eunit.hrl").

find_test_() ->
	[
		f(<<"abcde">>, <<"abcd">>, []),
		f(<<"abcde">>, <<"abcde">>, [{0,5}]),
		f(<<"abcde">>, <<"abcdeabcde">>, [{0,5}, {5,5}]),
		f(<<"abcde">>, <<"abcabcde">>, [{3,5}]),
		f(<<"aaa">>, <<"aaaa">>, [{0,3}, {1,3}]),
		f(<<"aaa">>, <<"nnaabaabaaaaa">>, [{8,3}, {9,3}, {10,3}])
	].


empty_str_test_() ->
	f(<<"any">>, <<>>, []).


f(Pattern, Str, Positions) ->
	?_assertEqual(Positions, find(Pattern, Str)).


empty_pattern_test_() ->
	?_assertThrow(
		'bad argument',
		find(<<>>, <<"any">>)).