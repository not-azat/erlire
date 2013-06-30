-module(bitap_tests).
-import(bitap, [find/2, find/3]).
-include_lib("eunit/include/eunit.hrl").

find_test_() ->
	[
		f0(<<"abcde">>, <<"abcd">>, []),
		f0(<<"abcde">>, <<"abcde">>, [{0,5,0}]),
		f0(<<"abcde">>, <<"abcdeabcde">>, [{0,5,0}, {5,5,0}]),
		f0(<<"abcde">>, <<"abcabcde">>, [{3,5,0}]),
		f0(<<"aaa">>, <<"aaaa">>, [{0,3,0}, {1,3,0}]),
		f0(<<"aaa">>, <<"nnaabaabaaaaa">>, [{8,3,0}, {9,3,0}, {10,3,0}])
	].

find_k_test_() ->
	[
		fk(<<"abcde">>, <<"abcd">>, 1, have),
		fk(<<"abcde">>, <<"ab">>, 1, not_have),
		fk(<<"abcd">>, <<"asbfced">>, 1, not_have),
		fk(<<"abcd">>, <<"asbfced">>, 2, not_have),
		fk(<<"abcd">>, <<"asbfced">>, 3, have),
		fk(<<"abc">>, <<"aff">>, 1, not_have)
	].


empty_str_test_() ->
	f0(<<"any">>, <<>>, []).


f0(Pattern, Str, Positions) ->
	?_assertEqual(Positions, find(Pattern, Str)).

fk(Pattern, Str, K, have) ->
	?_assertNotEqual([], find(Pattern, Str, K));
fk(Pattern, Str, K, not_have) ->
	?_assertEqual([], find(Pattern, Str, K)).


empty_pattern_test_() ->
	?_assertThrow(
		'bad argument',
		find(<<>>, <<"any">>)).