-module(re_transition_tests).
-import(re_transition, [convert_automata/1, find_transition/2, compose/2, matches/2]).
-include_lib("eunit/include/eunit.hrl").


concat_test_() ->
	[
		check_all_variants(<<"ab?cd">>, <<"abcd">>, [<<"abcd">>]),
		check_all_variants(<<"a?b?c?d?">>, <<"abcde">>, [<<"a">>, <<"ab">>, <<"abc">>, <<"abcd">>]),
		check_all_variants(<<"ab(cd?)*">>, <<"abcdccd">>, [
			<<"ab">>, 
			<<"abc">>, 
			<<"abcd">>, 
			<<"abcdc">>, 
			<<"abcdcc">>, 
			<<"abcdccd">>]),
		check_all_variants(<<"(ab|c)+">>, <<"abcab">>, [<<"ab">>, <<"abc">>, <<"abcab">>])].


empty_transition_test_() ->
	Automata = re_compiler:compile(<<"#">>),
	TransitionAutomata = convert_automata(Automata),
	Transition = find_transition(<<"d">>, TransitionAutomata),
	?_assertNot(matches(Transition, TransitionAutomata)).


%% For example: for input string <<"abcde">>
%% test tries to match following strings, composing letters one by one:
%% <<"a">> <<"ab">> <<"abc">> <<"abcd">> <<"abcde">>
%% ShouldMatchList should contain those and only those variants, that should match,
%% for example [<<"ab">>, <<"abc">>] for RegExBin <<"abc*">>
check_all_variants(RegExBin, BinStrToRecompose, ShouldMatchList) ->
	Automata = re_compiler:compile(RegExBin),
	TransitionAutomata = convert_automata(Automata),
	AllMatchedList = check_all_variants_acc1(TransitionAutomata, BinStrToRecompose),
	Set1 = gb_sets:from_list(AllMatchedList),
	Set2 = gb_sets:from_list(ShouldMatchList),
	CommentList = << <<M/binary, ", ">> || M <- AllMatchedList >>,
	Comment = <<RegExBin/binary, " ", BinStrToRecompose/binary, "[", CommentList/binary, "]">>,
	[
		{Comment, ?_assert(gb_sets:is_subset(Set1, Set2))}, 
		{Comment, ?_assert(gb_sets:is_subset(Set2, Set1))}
	].

check_all_variants_acc1(_, <<>>) ->
	[];
check_all_variants_acc1(TransitionAutomata, <<Letter:1/binary, Rest/binary>>) ->
	LetterTransition = find_transition(Letter, TransitionAutomata),
	MatchedAcc = case matches(LetterTransition, TransitionAutomata) of 
		true -> [Letter];
		false -> []
	end,
	check_all_variants_acc2(TransitionAutomata, Rest, Letter, MatchedAcc, LetterTransition).

check_all_variants_acc2(_, <<>>, _, MatchedAcc, _) ->
	MatchedAcc;
check_all_variants_acc2(TransitionAutomata, <<Letter:1/binary, Rest/binary>>, BinStrAcc, MatchedAcc, TransitionAcc) ->
	Transition = find_transition(Letter, TransitionAutomata),
	TransitionAcc1 = compose(TransitionAcc, Transition),
	BinStrAcc1 = <<BinStrAcc/binary, Letter/binary>>,
	MatchedAcc1 = case matches(TransitionAcc1, TransitionAutomata) of 
		true -> [BinStrAcc1 | MatchedAcc];
		false -> MatchedAcc
	end,
	check_all_variants_acc2(TransitionAutomata, Rest, BinStrAcc1, MatchedAcc1, TransitionAcc1).



