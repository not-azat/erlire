-module(re_transition).
-export([convert_automata/1, find_transition/2, compose_transitions/2, matches/2]).
-export([test/0]).


-type transition_automata() :: {transition_automata, InitialStates :: gb_set(), FinalStates :: gb_set(), dict()}.
%% where dict(Letter => dict(Index => set(Index)))

-type re_automata() :: {re_automata, InitialExpNum::integer(), dict()}.
%% where dict(Index => {IsFinal::boolean(), dict(Letter => set(Index))})

-type transition() :: dict().
%% where dict(Index => set(Index))


%% public
-spec convert_automata(re_automata()) -> transition_automata().

convert_automata({re_automata, InitialExpNum, AutomataDict}) ->
	AutomataDictNoIsFinal = dict:map(
		fun(_, {_, Value}) -> Value end,
		AutomataDict),
	TransAutoDict = swap_first_and_second_key(
		AutomataDictNoIsFinal,
		fun(_, Set1, Set2) ->
			gb_sets:union(Set1, Set2)
		end),
	InitialStates = gb_sets:singleton(InitialExpNum),
	FinalStates = dict:fold(
		fun
			(Index, {true, _}, AccSet) -> gb_sets:add(Index, AccSet);
			(_, {false, _}, AccSet) -> AccSet
		end,
		gb_sets:new(),
		AutomataDict),
	{transition_automata, InitialStates, FinalStates, TransAutoDict}.


-spec swap_first_and_second_key(
		dict(), 
		MergeValues::fun((term(), term(), term()) -> term())) -> dict().
%% dict(First => dict(Second => Value))  
%% to
%% dict(Second => didict:dict()ct(First => Value))
swap_first_and_second_key(First2Second2Value, MergeValues) ->
	dict:fold(
		fun(First, Second2Value, Second2First2ValueAcc) ->
			SwappedPairsList = make_all_swapped_pairs(First, Second2Value), % [{Second, dict(First => Value)}]
			add_all_swapped_pairs(SwappedPairsList, Second2First2ValueAcc, MergeValues)
		end,
		dict:new(),
		First2Second2Value).


-spec make_all_swapped_pairs(
		First::term(), 
		Second2Value :: dict()) -> list({Second::term(), First2Value :: dict()}).

make_all_swapped_pairs(First, Second2Value) ->
	dict:fold(
		fun(Second, Value, AccList) ->
			First2Value = dict:store(First, Value, dict:new()),
			[{Second, First2Value} | AccList]
		end,
		[],
		Second2Value).


-spec add_all_swapped_pairs(
		list({Key :: term(), Value :: dict()}), 
		Acc :: dict(), 
		fun((term(), term(), term()) -> term())) -> dict().

add_all_swapped_pairs(KVList, AccDict, MergeValues) ->
	lists:foldl(
		fun({Key, Value1}, InnerAccDict) ->
			case dict:find(Key, InnerAccDict) of 
				{ok, Value2} ->
					dict:store(Key, dict:merge(MergeValues, Value1, Value2), InnerAccDict);
				error ->
					dict:store(Key, Value1, InnerAccDict)
			end
		end,
		AccDict,
		KVList).


%% public
-spec find_transition(binary(), transition_automata()) -> transition().

find_transition(<<>>, _) ->
	dict:new();
find_transition(<<Letter:1/binary, Rest/binary>>, {transition_automata, _, _, TransAutoDict}) ->
	case dict:find(Letter, TransAutoDict) of 
		{ok, Value} -> 
			find_transition_acc(Rest, TransAutoDict, Value);
		error -> 
			dict:new()
	end.


-spec find_transition_acc(
		binary(), 
		TransAutoDict :: dict(), 
		Transition :: dict()) -> Transition :: dict().

find_transition_acc(<<>>, _, AccDict) ->
	AccDict;
find_transition_acc(<<Letter:1/binary, Rest/binary>>, TransAutoDict, AccDict) ->
	AccDict1 = case dict:find(Letter, TransAutoDict) of 
		{ok, Value} -> compose_transitions(AccDict, Value);
		error -> dict:new()
	end,
	find_transition_acc(Rest, TransAutoDict, AccDict1).


%% public
-spec compose_transitions(transition(), transition()) -> transition().

compose_transitions(Transition1, Transition2) ->
	dict:fold(
		fun(Left, MiddleLeftSet, AccDict) ->
			gb_sets:fold(
				fun(MiddleLeft, InnerAccDict) ->
					case dict:find(MiddleLeft, Transition2) of 
						{ok, RightSet} -> dict:store(Left, RightSet, InnerAccDict);
						error -> InnerAccDict
					end
				end,
				AccDict,
				MiddleLeftSet)
		end,
		dict:new(),
		Transition1).


%% public
-spec matches(transition(), transition_automata()) -> boolean().

matches(Transition, {transition_automata, InitialStates, FinalStates, _}) ->
	dict:fold(
		fun(Index, IndexSet, IsFound) ->
			HasInitial = gb_sets:is_element(Index, InitialStates),
			HasFinal = (gb_sets:size(gb_sets:intersection(IndexSet, FinalStates)) > 0),
			HasInitial and HasFinal or IsFound
		end,
		false,
		Transition).


%%%   TESTS   %%%

test() ->
	swap_first_and_second_key_test(),
	check_all_variants(<<"ab?cd">>, <<"abcd">>, [<<"abcd">>]),
	check_all_variants(<<"a?b?c?d?">>, <<"abcde">>, [<<"a">>, <<"ab">>, <<"abc">>, <<"abcd">>]),
	check_all_variants(<<"ab|(cd?)*">>, <<"abcdccd">>, [
		<<"ab">>, 
		<<"abc">>, 
		<<"abcd">>, 
		<<"abcdc">>, 
		<<"abcdcc">>, 
		<<"abcdccd">>]),
	check_all_variants(<<"(ab|c)+">>, <<"abcab">>, [<<"ab">>, <<"abc">>, <<"abcab">>]).


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
	true = gb_sets:is_subset(Set1, Set2),
	true = gb_sets:is_subset(Set2, Set2),
	ok.

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
	TransitionAcc1 = compose_transitions(TransitionAcc, Transition),
	BinStrAcc1 = <<BinStrAcc/binary, Letter/binary>>,
	MatchedAcc1 = case matches(TransitionAcc1, TransitionAutomata) of 
		true -> [BinStrAcc1 | MatchedAcc];
		false -> MatchedAcc
	end,
	check_all_variants_acc2(TransitionAutomata, Rest, BinStrAcc1, MatchedAcc1, TransitionAcc1).


%% dict(First => dict(Second => Value))  
%% to
%% dict(Second => dict(First => Value))
swap_first_and_second_key_test() ->
	Second2Value1 = dict:from_list([
		{sec1, [val1, val2]}, 
		{sec2, [val3, val4]}, 
		{sec3, [val5, val6]}]),
	Second2Value2 = dict:from_list([
		{sec2, [val7, val8]}, 
		{sec3, [val9, val10]}, 
		{sec4, [val11, val12]}]),
	First2Second2Value = dict:from_list([
		{"fir1", Second2Value1},
		{"fir2", Second2Value2}]),
	MergeValues = fun(_, List1, List2) -> List1 ++ List2 end,
	Second2First2Value = swap_first_and_second_key(First2Second2Value, MergeValues),
	test_for_key_and_list(sec1, [val1, val2], Second2First2Value, MergeValues),
	test_for_key_and_list(sec2, [val3, val4, val7, val8], Second2First2Value, MergeValues),
	test_for_key_and_list(sec3, [val5, val6, val9, val10], Second2First2Value, MergeValues),
	test_for_key_and_list(sec4, [val11, val12], Second2First2Value, MergeValues).


test_for_key_and_list(Key, List, Second2First2Value, MergeValues) ->
	RightSet = gb_sets:from_list(List),
	{ok, First2Value} = dict:find(Key, Second2First2Value),
	%io:format("First2Value: ~p~n", [First2Value]),
	FoundList = lists:foldl(
		fun({K, V}, Acc) -> MergeValues(K, V, Acc) end,
		[], 
		dict:to_list(First2Value)),
	FoundSet = gb_sets:from_list(FoundList),
	%io:format("FoundSet: ~p~n", [gb_sets:to_list(FoundSet)]),
	%io:format("RightSet: ~p~n", [gb_sets:to_list(RightSet)]),
	true = gb_sets:is_subset(FoundSet, RightSet),
	true = gb_sets:is_subset(RightSet, FoundSet),
	ok.


%%% DEBUGGING %%%
% print_transition_automata({transition_automata, InitialStates, FinalStates, TransAutoDict}) ->
% 	io:format("transition_automata:~n"),
% 	io:format("InitialStates: ~p~n", gb_sets:to_list(InitialStates)),
% 	io:format("FinalStates: ~p~n", gb_sets:to_list(FinalStates)),
% 	io:format("TransAutoDict:~n"),
% 	print_transition_dict(TransAutoDict).


% print_transition_dict(TransAutoDict) ->
% 	dict:map(
% 		fun(Letter, LetterDict) ->
% 			print_letter_dict(Letter, LetterDict)
% 		end,
% 		TransAutoDict).

% print_letter_dict(Letter, LetterDict) ->
% 	io:format("~p:~n", [Letter]),
% 	dict:map(
% 		fun(LeftState, RightStatesSet) ->
% 			io:format("~p => ~p~n", [LeftState, gb_sets:to_list(RightStatesSet)])
% 		end,
% 		LetterDict),
% 	ok.


