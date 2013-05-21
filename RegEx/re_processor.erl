-module(re_processor).
-export([run/2]).
-include("re_types.hrl").


-spec run(binary(), re_compiler:re_automata()) -> boolean().

run(BinString, Automata) ->
	matches(BinString, Automata, re_compiler:initial_states_set(Automata)).


matches(<<>>, Automata, States) ->
	is_matched(Automata, States);

matches(<<Token:1/binary, Rest/binary>>, Automata, States) ->
	case (is_matched(Automata, States)) of 
		true -> true;
		false ->
			NewStates = move_states(Token, Automata, States),
			matches(Rest, Automata, NewStates)
	end.


is_matched(Automata, CurrentStatesSet) ->
	gb_sets:fold(
		fun(LIndex, Acc) ->
			case (re_compiler:is_matched(LIndex, Automata)) of 
				true -> true;
				false -> Acc
			end 
		end,
		false,
		CurrentStatesSet).


move_states(Token, Automata, StatesSet) ->
	gb_sets:fold(
		fun(Index, CurrentStatesSet) ->
			States = re_compiler:next_states_set(Token, Index, Automata),
			gb_sets:union([CurrentStatesSet, States, re_compiler:initial_states_set(Automata)])
		end,
		gb_sets:new(),
		StatesSet).
