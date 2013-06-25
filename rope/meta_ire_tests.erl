-module(meta_ire_tests).
-include_lib("eunit/include/eunit.hrl").
-import(meta_ire, [new/3, matches/1, split/2, merge/2]).


rope_matches_test_() ->
	lists:map(fun(Num) ->
			{ok, Bin} = file:read_file(io_lib:format("test_data/meta_ire_test_~p.txt", [Num])),
			Automata = re_compiler:compile(<<"a*((bao)+|f)c*">>),
			TransitionAutomata = re_transition:convert_automata(Automata),
			MetaIre = new(Bin, TransitionAutomata, 2000),
			?_assert(matches(MetaIre))
		end,
		lists:seq(1, 2)).

rope_not_matches_test_() ->
	lists:map(fun(Num) ->
			{ok, Bin} = file:read_file(io_lib:format("test_data/meta_ire_test_n_~p.txt", [Num])),
			Automata = re_compiler:compile(<<"a*((bao)+|f)c*">>),
			TransitionAutomata = re_transition:convert_automata(Automata),
			MetaIre = new(Bin, TransitionAutomata, 2000),
			?_assert(not matches(MetaIre))
		end,
		lists:seq(1, 2)).

split_test_() ->
	{ok, Bin} = file:read_file("test_data/meta_ire_test_3.txt"),
	<<Bin1:1000/binary, Bin2/binary>> = Bin,
	Automata = re_compiler:compile(<<"a*((bao)+|f)c*">>),
	TransitionAutomata = re_transition:convert_automata(Automata),
	Ire1 = new(Bin1, TransitionAutomata, 200),
	Ire2 = new(Bin2, TransitionAutomata, 200),
	Ire = new(Bin, TransitionAutomata, 200),
	{Ire11, Ire12} = split(Ire, 1000),
	M1 = matches(Ire1),
	M2 = matches(Ire2),
	M11 = matches(Ire11),
	M12 = matches(Ire12),
	[?_assertEqual(M1, M11), ?_assertEqual(M2, M12)].

merge_test_() ->
	{ok, Bin} = file:read_file("test_data/meta_ire_test_3.txt"),
	<<Bin1:1000/binary, Bin2/binary>> = Bin,
	Automata = re_compiler:compile(<<"a*((bao)+|f)c*">>),
	TransitionAutomata = re_transition:convert_automata(Automata),
	Ire1 = new(Bin1, TransitionAutomata, 200),
	Ire2 = new(Bin2, TransitionAutomata, 200),
	Ire = merge(Ire1, Ire2),
	IreFull = new(Bin, TransitionAutomata, 250),
	?_assertEqual(matches(Ire), matches(IreFull)).



