-module(re_transition).
-compile([export_all]).
-include("re_types.hrl").



compute_transition_dict({re_automata, _, AutomataDict}) ->
	dict:fold(
		fun(StateIndex, {_, EdgesDict}, ResultAcc) ->
			add_letters_to_transition_dict(StateIndex, EdgesDict, ResultAcc)
		end,
		dict:new(),
		AutomataDict).


add_letters_to_transition_dict(LeftState, EdgesDict, TransitionDict) ->
	dict:fold(
		fun(Letter, SetOfIndices, TransitionDictAcc) ->
			LetterDict = case (dict:find(Letter, TransitionDictAcc)) of 
				error -> dict:new();
				{ok, FoundLetterDict} -> FoundLetterDict
			end,
			LetterDict1 = add_states_to_letter_dict(LeftState, SetOfIndices, LetterDict),
			dict:store(Letter, LetterDict1, TransitionDictAcc)
		end, 
		TransitionDict,
		EdgesDict).


add_states_to_letter_dict(LeftState, SetOfIndices, LetterDict) ->
	case (dict:find(LeftState, LetterDict)) of 
		error ->
			dict:store(LeftState, SetOfIndices, LetterDict);
		{ok, FoundSet} ->
			dict:store(LeftState, gb_sets:union(FoundSet, SetOfIndices), LetterDict)
	end.




print_transition_dict(TransitionDict) ->
	dict:map(
		fun(Letter, LetterDict) ->
			print_letter_dict(Letter, LetterDict)
		end,
		TransitionDict).

print_letter_dict(Letter, LetterDict) ->
	io:format("<<~p>>:~n", [Letter]),
	dict:map(
		fun(LeftState, RightStatesSet) ->
			io:format("~p => ~p~n", [LeftState, gb_sets:to_list(RightStatesSet)])
		end,
		LetterDict),
	ok.




-spec compose(transition(), transition()) -> transition().

compose({String1, Dict1}, {String2, Dict2}) ->	
	{<<String1/binary, String2/binary>>, compose_letter_dicts(Dict1, Dict2)}.


compose_letter_dicts(LetterDict1, LetterDict2) ->
	dict:map(
		fun(_LeftState, LeftStatesSet) ->
			get_states_for(LeftStatesSet, LetterDict2)
		end,
		LetterDict1).

get_states_for(LeftStatesSet, LetterDict2) ->
	gb_sets:fold(
		fun(LeftSetState, ResultSetAcc) ->
			case dict:find(LeftSetState, LetterDict2) of 
				error ->
					ResultSetAcc;
				{ok, FoundSet} ->
					gb_sets:union(ResultSetAcc, FoundSet)
			end
		end,
		gb_sets:new(),
		LeftStatesSet).


% merge_letter_dicts(LetterDict1, LetterDict2) ->
% 	dict:merge(
% 		fun(_, StatesSet1, StatesSet2) ->
% 			gb_sets:union(StatesSet1, StatesSet2)
% 		end,
% 		LetterDict1,
% 		LetterDict2).



-spec new(letter(), Automata::re_automata()) -> transition().

new(Letter, TransitionDict) ->
	case dict:find(Letter, TransitionDict) of
		{ok, LetterDict} ->
			{Letter, LetterDict};
		error -> badarg
	end.



-spec match(string(), transition(), re_automata()) -> boolean().

match(String, {String, TransitionDict}, Automata) ->
	has_initial_and_final_state({String, TransitionDict}, Automata);
match(_, _, _) -> false. % even string doesn't match


has_initial_and_final_state({_, TransitionDict}, Automata = {re_automata, InitialStateIndex, _}) ->
	HasInitial = dict:is_key(InitialStateIndex, TransitionDict),
	HasFinal = dict:fold(
		fun(_, SetOfStates, LHasFinal) ->
			LHasFinal1 = gb_sets:fold(
				fun(RightState, LLHasFinal) ->
					LLHasFinal1 = re_compiler:is_matched(RightState, Automata),
					LLHasFinal or LLHasFinal1
				end,
				false,
				SetOfStates),
			LHasFinal or LHasFinal1
		end,
		false,
		TransitionDict),
	HasInitial and HasFinal.


test() ->
	ReStr = <<"ab|cd">>,
	Str1 = <<"a">>,
	Str2 = <<"b">>,
	Str3 = <<"c">>,
	Str4 = <<"d">>,
	Str12 = <<Str1/binary, Str2/binary>>,
	Str123 = <<Str12/binary, Str3/binary>>,
	Str1234 = <<Str123/binary, Str4/binary>>,
	Str34 = <<Str3/binary, Str4/binary>>,
	Auto = re_compiler:compile(ReStr),
	TrDict = compute_transition_dict(Auto),
	T1 = new(Str1, TrDict),
	T2 = new(Str2, TrDict),
	T3 = new(Str3, TrDict),
	T4 = new(Str4, TrDict),
	T12 = compose(T1, T2),
	T123 = compose(T12, T3),
	T1234 = compose(T123, T4),
	T34 = compose(T3, T4),
	Res1 = has_initial_and_final_state(T1, Auto),
	Res2 = has_initial_and_final_state(T2, Auto),
	Res3 = has_initial_and_final_state(T3, Auto),
	Res12 = has_initial_and_final_state(T12, Auto),
	Res123 = has_initial_and_final_state(T123, Auto),
	Res1234 = has_initial_and_final_state(T1234, Auto),
	Res34 = has_initial_and_final_state(T34, Auto),
	io:format("Str: ~s\tTrans: ~p\t Res: ~p~n", [Str1, T1, Res1]),
	io:format("Str: ~s\tTrans: ~p\t Res: ~p~n", [Str2, T2, Res2]),
	io:format("Str: ~s\tTrans: ~p\t Res: ~p~n", [Str3, T3, Res3]),
	io:format("Str: ~s\tTrans: ~p\t Res: ~p~n", [Str12, T12, Res12]),
	io:format("Str: ~s\tTrans: ~p\t Res: ~p~n", [Str123, T123, Res123]),
	io:format("Str: ~s\tTrans: ~p\t Res: ~p~n", [Str1234, T1234, Res1234]),
	io:format("Str: ~s\tTrans: ~p\t Res: ~p~n", [Str34, T34, Res34]).






