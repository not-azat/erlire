-module(meta_ire_tests).
-include_lib("eunit/include/eunit.hrl").
-import(meta_ire, [new/3, matches/1]).


rope_matches_test_() ->
	lists:map(fun(Num) ->
			{ok, Bin} = file:read_file(io_lib:format("test_data/meta_ire_test_~p.txt", [Num])),
			Automata = re_compiler:compile(<<"a*((bao)+|f)c*">>),
			TransitionAutomata = re_transition:convert_automata(Automata),
			MetaIre = new(Bin, TransitionAutomata, 50),
			?_assert(matches(MetaIre))
		end,
		lists:seq(1, 2)).

rope_not_matches_test_() ->
	lists:map(fun(Num) ->
			{ok, Bin} = file:read_file(io_lib:format("test_data/meta_ire_test_n_~p.txt", [Num])),
			Automata = re_compiler:compile(<<"a*((bao)+|f)c*">>),
			TransitionAutomata = re_transition:convert_automata(Automata),
			MetaIre = new(Bin, TransitionAutomata, 50),
			?_assert(not matches(MetaIre))
		end,
		lists:seq(1, 2)).