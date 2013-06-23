-module(utils_tests).
-import(utils, [swap_first_and_second_key/1]).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).


%%% swap_first_and_second_key %%%%

empty_test_() ->
	D1 = dict:new(),
	D2 = swap_first_and_second_key(D1),
	?_assertEqual([], dict:to_list(D2)).

inner_empty_test_() ->
	D1 = dict:new(),
	D2 = dict:from_list([{one, D1}]),
	D3 = swap_first_and_second_key(D2),
	?_assertEqual([], dict:to_list(D3)).

one_first_test_() ->
	D = dict:from_list([
		{one, dict:from_list([
			{1, a},
			{2, b}])}]),
	DR = swap_first_and_second_key(D),
	R = dict:from_list([
		{1, dict:from_list([{one, a}])},
		{2, dict:from_list([{one, b}])}]),
	?_assert(dict_equal(DR, R)).

bigger_test_() ->
	D = dict:from_list([
		{1, dict:from_list([
			{one, [a]},
			{two, [b,c]},
			{three, [a,b,c]}])},
		{2, dict:from_list([
			{one, [a]},
			{two, [b,c]}])}]),
	DR = swap_first_and_second_key(D),
	R = dict:from_list([
			{one, dict:from_list([
					{1, [a]},
					{2, [a]}
				])},
			{two, dict:from_list([
					{1, [b, c]},
					{2, [b, c]}
				])},
			{three, dict:from_list([
					{1, [a,b,c]}
				])}]),
	?_assert(dict_equal(DR, R)).

%% to compare two dict(term => dict(term => term))
dict_equal(D1, D2) ->
	try
		L1 = lists:sort(dict:to_list(D1)),
		L2 = lists:sort(dict:to_list(D2)),
		L11 = lists:map(
			fun({K, VDict}) ->
				{K, lists:sort(dict:to_list(VDict))}
			end,
			L1),
		L21 = lists:map(
			fun({K, VDict}) ->
				{K, lists:sort(dict:to_list(VDict))}
			end,
			L2),
		L11 =:= L21
	catch 
		error:{badrecord, dict} -> false
	end.






