-module(re_processor_tests).
-include_lib("eunit/include/eunit.hrl").

%% string, regex, manually_reversed_regex
regex_and_reversed_as_re_test_() ->
	[
		eq(<<"d">>, 
			<<"((d))">>, 
			<<"((d))">>),
		eq(<<"aaaaaaa">>, 
			<<"aaaa?a?a?a?a?a?a?">>,
			<<"a?a?a?a?a?a?a?aaa">>),
		eq(<<"ba">>, 
			<<"a*b?(a|bc)">>,
			<<"(a|cb)b?a*">>),
		eq(<<"0d0">>, 
			<<"(1|2|3|4|5|6|7|8|9|0)+d(1|2|3|4|5|6|7|8|9|0)+">>,
			<<"(1|2|3|4|5|6|7|8|9|0)+d(1|2|3|4|5|6|7|8|9|0)+">>),
		eq(<<"mo">>, 
			<<"m.?">>,
			<<".?m">>),
		eq(<<"2+2=4?">>, 
			<<"2\\+2=4\\?">>,
			<<"\\?4=2\\+2">>),
		eq(<<"">>, 
			<<"">>,
			<<"">>),
		eq(<<"a">>, 
			<<".">>,
			<<".">>),
		eq(<<"">>, 
			<<".*">>,
			<<"">>),
		eq(<<"321()">>, 
			<<".*">>,
			<<".*">>),
		eq(<<"()">>, 
			<<"(..)?">>,
			<<"(..)?">>),
		eq(<<"">>, 
			<<"((d|s)(ab))?">>,
			<<"((ba)(d|s))?">>),
		eq(<<"">>, 
			<<"a(bc)d">>,
			<<"d(cb)a">>),
		eq(<<"(abc)">>, 
			<<"\\(abc\\)">>,
			<<"\\)cba\\(">>),
		eq(<<"abc">>, 
			<<"\\(abc\\)">>,
			<<"\\)cba\\(">>)
	].


eq(BinStr, BinRegExStr, BinRegExStrR) ->
	BinStrR = reverse(BinStr),
	[	?_assertEqual(re_matches(BinStr, BinRegExStr), matches(BinStr, BinRegExStr)),
		?_assertEqual(re_matches(BinStrR, BinRegExStrR), matches_r(BinStr, BinRegExStr)),
		?_assertEqual(matches(BinStrR, BinRegExStrR), matches_r(BinStr, BinRegExStr))].


reverse(Binary) ->
	reverse(Binary, <<>>).
reverse(<<>>, Acc) ->
	Acc;
reverse(<<H, Rest/binary>>, Acc) ->
	reverse(Rest, <<H, Acc/binary>>).


re_matches(Str, RegEx) ->
	{ok, MP} = re:compile(RegEx),
	case (re:run(Str, MP)) of 
		{match, _} -> true;
		nomatch -> false
	end.

matches(Str, RegEx) ->
	re_processor:run(Str, re_compiler:compile(RegEx)).

matches_r(Str, RegEx) ->
	{Auto, RAuto} = re_compiler:compile_r(RegEx),
	re_processor:run(reverse(Str), RAuto).

