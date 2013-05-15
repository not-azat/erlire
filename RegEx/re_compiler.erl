-module(re_compiler).
-export([compile/1, initial_states_set/1, next_states_set/3, is_matched/2]).
-include("re_types.hrl").

%%% public interface

-spec compile(regex_post()) -> re_automata() | badarg.

compile(RegExStr) ->
	PostfixRegExStr = re_infix_to_postfix:convert(RegExStr),
	case (PostfixRegExStr) of 
		badarg -> badarg;
		_ ->
			compact(process_token(PostfixRegExStr, [], dict:new()))
	end.

%%% interface for re_transition

% -spec compute_transition(letter(), re_automata()) -> transition().

% compute_transition(Letter, {re_automata, InitialExpNum, AutomataDict}) ->
% 	dict:fold(
% 		fun(StateIndex, {IsFinal, EdgesDict}, ResultAcc) ->
% 			add_states_for_letter(Letter, EdgesDict, ResultAcc)
% 		end,
% 		dict:new(),
% 		AutomataDict).

% add_states_for_letter(Letter, EdgesDict, Acc) ->
% 	dict:fold(
% 		fun
% 			(Letter, SetOfIndices, LAcc) ->
% 				gb_sets:union(SetOfIndices, LAcc);
% 			(_, _, LAcc) -> 
% 				LAcc
% 		end,
% 		Acc,
% 		EdgesDict).


%%% interface for re_processor


-spec initial_states_set(re_automata()) -> gb_set().

initial_states_set({re_automata, InitialExpNum, _}) ->
	gb_sets:add(InitialExpNum, gb_sets:new()).


-spec next_states_set(letter(), exp_index(), re_automata()) -> {error, invalid_state_index} | gb_set().

next_states_set(Token, CurrentState, {re_automata, _InitialExpNum, AutomataDict}) ->
	case (dict:find(CurrentState, AutomataDict)) of 
		error ->
			{error, invalid_state_index};
		{ok, {_, EdgesDict}} ->
			case (dict:find(Token, EdgesDict)) of 
				{ok, Set} -> Set;
				error -> gb_sets:new()
			end 
	end.


-spec is_matched(exp_index(), re_automata()) -> true | false | {error, invalid_state_index}.

is_matched(CurrentState, {re_automata, _InitialExpNum, AutomataDict}) ->
	case (dict:find(CurrentState, AutomataDict)) of 
		{ok, {true, _}} -> true;
		error -> {error, invalid_state_index};
		{ok, {false, _}} -> false 
	end.


%%% Compilation to NFA with empty edges

-spec process_token(list(letter()), list(exp_index()), dict()) -> re_automata1().

process_token([], [], _) ->
	{re_automata1, 0, dict:new()};

process_token([], [ExpressionNum], Expressions) ->
	enclose_to_matched(ExpressionNum, Expressions);

process_token([Token | RestTokens], ArgStack, Expressions) ->
	case (re_lang:is_operator(Token)) of 
		false ->
			{ExpNum, NewExpressions} = create_letter_expression(Token, Expressions),
			process_token(RestTokens, [ExpNum | ArgStack], NewExpressions);
		true ->
			process_operator(Token, RestTokens, ArgStack, Expressions)
	end.



-spec process_operator(operator(), list(letter()), list(exp_index()), dict()) -> re_automata1().

process_operator($., RestTokens, [ExpNum1, ExpNum2 | RestStack], Expressions) ->
	process_token(
		RestTokens,
		[ExpNum2 | RestStack],
		cat_expressions(ExpNum2, ExpNum1, Expressions));

process_operator($|, RestTokens, [ExpNum1, ExpNum2 | RestStack], Expressions) ->
	{NewExpNum, NewExpressions} = create_split_expression(ExpNum1, ExpNum2, Expressions),
	process_token(
		RestTokens,
		[NewExpNum | RestStack],
		NewExpressions);

process_operator($?, RestTokens, [ExpNum | RestStack], Expressions) ->
	{NewExpNum, NewExpressions} = create_split_expression(ExpNum, null, Expressions),
	process_token(
		RestTokens,
		[NewExpNum | RestStack],
		NewExpressions);

process_operator($*, RestTokens, [ExpNum | RestStack], Expressions) ->
	{NewExpNum, NewExpressions} = create_split_expression(ExpNum, null, Expressions),
	process_token(
		RestTokens,
		[NewExpNum | RestStack],
		cat_expressions(ExpNum, NewExpNum, NewExpressions));

process_operator($+, RestTokens, [ExpNum | RestStack], Expressions) ->
	{NewExpNum, NewExpressions} = create_split_expression(ExpNum, null, Expressions),
	process_token(
		RestTokens,
		[ExpNum | RestStack],
		cat_expressions(ExpNum, NewExpNum, NewExpressions)).



-spec create_split_expression(Out1::exp_index(), Out2::exp_index(), Expressions::dict()) -> {exp_index(), dict()}.
% creates and adds to Expressions new 'split' expression with specified Out1 and Out2

create_split_expression(null, null, Expressions) ->
	Index = dict:size(Expressions), % new state number: before optimizations (such as 'split' deletions) Expressions dict only grows
	Exp = {{split, null, null}, [Index]},
	{Index, dict:store(Index, Exp, Expressions)};

create_split_expression(ExpNum1, null, Expressions) ->
	Index = dict:size(Expressions), % new state number
	{ok, {_, OutgoingExpressions1}} = dict:find(ExpNum1, Expressions),
	Exp = {{split, ExpNum1, null}, [Index | OutgoingExpressions1]},
	{Index, dict:store(Index, Exp, Expressions)};

create_split_expression(ExpNum1, ExpNum2, Expressions) ->
	Index = dict:size(Expressions), % new state number
	{ok, {_, OutgoingExpressions1}} = dict:find(ExpNum1, Expressions),
	{ok, {_, OutgoingExpressions2}} = dict:find(ExpNum2, Expressions),
	Exp = {{split, ExpNum1, ExpNum2}, OutgoingExpressions1 ++ OutgoingExpressions2},
	{Index, dict:store(Index, Exp, Expressions)}.



-spec cat_expressions(exp_index(), exp_index(), dict()) -> dict().
% concatenate two expressions

cat_expressions(ExpNum1, ExpNum2, Expressions) ->
	{ok, {_, Links1}} = dict:find(ExpNum1, Expressions),
	% add ExpNum2 expression to every front node of ExpNum1 expression
	Expressions1 = lists:foldl(
		fun(LIndex, LExpressions)->
			{ok, Found} = dict:find(LIndex, LExpressions),
			case (Found) of 
				{{letter, Letter, null}, _} ->
					dict:store(LIndex, {{letter, Letter, ExpNum2}, []}, LExpressions);
				{{split, null, null}, _} ->
					dict:store(LIndex, {{split, ExpNum2, ExpNum2}, []}, LExpressions);
				{{split, Out1, null}, _} ->
					dict:store(LIndex, {{split, Out1, ExpNum2}, []}, LExpressions);
				{_, _} ->
					LExpressions
			end
		end, 
		Expressions, 
		Links1),
	{ok, {_, OutgoingExpressions2}} = dict:find(ExpNum2, Expressions1),
	{ok, {Exp1, _}} = dict:find(ExpNum1, Expressions1),
	dict:store(ExpNum1, {Exp1, OutgoingExpressions2}, Expressions1).



-spec create_letter_expression(letter(), Expressions::dict()) -> {exp_index(), dict()}.
% creates and adds to Expressions new 'letter' expression with Out = null

create_letter_expression(Letter, Expressions) ->
	Index = dict:size(Expressions), % new state number
	Exp = {{letter, Letter, null}, [Index]}, % front expression of itself
	{Index, dict:store(Index, Exp, Expressions)}.



-spec enclose_to_matched(InitialExpNum::exp_index(), Expressions::dict()) -> re_automata1().
% creates 'matches' expression

enclose_to_matched(InitialExpNum, Expressions) ->
	Index = dict:size(Expressions), % new state number
	Exp = {matched, [Index]}, % front expression of itself
	{re_automata1,
		InitialExpNum, 
		dict:map(
			fun(_, {LExp, _}) -> LExp end, % remove list of front expressions (they needed only for expressions concatenation)
			cat_expressions(
				InitialExpNum, 
				Index, 
				dict:store(Index, Exp, Expressions)))}.



%%% Deleting empty edges in automata.
%%% So we need to connect all nodes with empty outgoing edges to those nodes, which we can reach with one non-empty edge
%%% Automata changes its representation to dict(vertexNum() =>  dict(Letter => list(vertexNum()) )) 


-spec compact(re_automata1()) -> re_automata().

compact({re_automata1, InitialExpNum, Automata}) ->
	case (dict:find(InitialExpNum, Automata)) of 
		error ->
			StatesDict = dict:store(InitialExpNum, {true, dict:new()}, dict:new()), % the only state is final
			{re_automata, InitialExpNum, StatesDict};
		{ok, Val} ->
			Automata1 = remove_redundant_states(compact(Automata, InitialExpNum, Val)),
			{re_automata, InitialExpNum, Automata1}
	end.



-spec compact 	(re_automata(), exp_index(), proto_exp()) -> dict();
				(re_automata1(), exp_index(), state()) -> dict();
				(re_automata(), exp_index(), state()) -> dict();
				(re_automata1(), exp_index(), proto_exp()) -> dict().

% % already compact state
compact(Automata, _, {proto_exp, _IsFinalState, _Destinations}) ->
	Automata;

compact(Automata, Index, State) ->
	% find {letter(), index()} pairs, but check if current state becomes final
	{IsFinalState, DestinationsList} = exclude_final(find_destinations(Automata, State)),
	DestinationsDict = destinations_list_to_dict(DestinationsList),
	Automata1 = dict:store(Index, {proto_exp, IsFinalState, DestinationsDict}, Automata),		
	dict:fold( % for each letter
		fun(_Letter, SetOfIndeces, LAutomata) ->
			gb_sets:fold( % for each outgoing edge with this letter
				fun(LLIndex, LLAutomata) ->
					{ok, Val} = dict:find(LLIndex, LLAutomata),
					compact(LLAutomata, LLIndex, Val)
				end,
				LAutomata,
				SetOfIndeces)
		end,
		Automata1,
		DestinationsDict).


-spec remove_redundant_states(dict()) -> dict(). % difference in value types

remove_redundant_states(Automata) ->
	Automata1 = dict:filter(
		fun
			(_, {proto_exp, _, _}) -> true;
			(_, _) -> false % that was not reached
		end,
		Automata),
	dict:map(
		fun(_, {proto_exp, IsMatched, TransDict}) -> {IsMatched, TransDict} end,
		Automata1).



-spec destinations_list_to_dict(list({letter(), exp_index()})) -> dict().

destinations_list_to_dict(List) ->
	destinations_list_to_dict(List, dict:new()).

destinations_list_to_dict([], Dict) ->
	Dict;
destinations_list_to_dict([{Letter, Index} | Rest], Dict) ->
	Dict1 = case (dict:find(Letter, Dict)) of 
		{ok, Set} -> 
			dict:store(Letter, gb_sets:add(Index, Set), Dict);
		error ->
			dict:store(Letter, gb_sets:add(Index, gb_sets:new()), Dict)
	end,
	destinations_list_to_dict(Rest, Dict1).	

destinations_dict_to_list(Dict) ->
	dict:fold(
		fun(Letter, SetOfIndeces, Acc) ->
			ListOfIndices = gb_sets:to_list(SetOfIndeces),
			Acc ++ lists:map(fun(Index) -> {Letter, Index} end, ListOfIndices)
		end,
		[],
		Dict).



-spec find_destinations	(dict(), state()) -> list({letter(), exp_index()} | final);
						(dict(), proto_exp()) -> list({letter(), exp_index()} | final).

% breadth-first search for nodes at one-letter distance (and if final state is at zero-letter distance, add 'final')
find_destinations(_, matched) ->
	[final];
find_destinations(_, already_watched) ->
	[]; % all outgoing edges are already taken
find_destinations(_, {letter, Letter, Index}) ->
	[{Letter, Index}];
find_destinations(Automata, {split, Index1, Index2}) ->
	{ok, State1} = dict:find(Index1, Automata),
	{ok, State2} = dict:find(Index2, Automata),
	Automata1 = dict:store(Index1, already_watched, Automata), % to avoid split cycles
	Automata2 = dict:store(Index2, already_watched, Automata1), % this 'watched' changes won't affect real automata, this is its "copy"
	Result = find_destinations(Automata2, State1) ++ find_destinations(Automata2, State2),
	Result;

find_destinations(_, {proto_exp, IsFinal, DestinationsDict}) ->
	OutgoingList = destinations_dict_to_list(DestinationsDict),
	case (IsFinal) of 
		true -> [final | OutgoingList];
		false -> OutgoingList
	end.


-spec exclude_final(list({letter(), exp_index()} | final)) -> list({letter(), exp_index()}).

exclude_final(List) -> 
	exclude_final(List, [], false).

exclude_final([], Result, IsFinalState) ->
	{IsFinalState, Result};
exclude_final([final | Rest], Result, _) ->
	exclude_final(Rest, Result, true);
exclude_final([Current | Rest], Result, IsFinalState) ->
	exclude_final(Rest, [Current | Result], IsFinalState).


% %%% Debugging & visualizing

% print_final_automata({re_automata, InitialExpNum, Dict}) ->
% 	io:format("Initial state: ~p~n", [InitialExpNum]),
% 	dict:map(fun
% 		(Index, {IsFinal, EdgesDict}) ->
% 			io:format("from state (final: ~p) ~p:~n", [IsFinal, Index]),
% 			dict:map(fun(Letter, SetOfStatesIndexes) ->
% 					io:format("~p ==> ~p~n", [Letter, gb_sets:to_list(SetOfStatesIndexes)])
% 				end,
% 				EdgesDict)
% 		end,
% 		Dict),
% 	ok.


% compile_and_print(InfixRegEx) ->
% 	print_final_automata(compile(re_infix_to_postfix:convert(InfixRegEx))).


% compile_and_print_old(InfixRegEx) ->
% 	PostfixRegEx = re_infix_to_postfix:convert(InfixRegEx),
% 	Auto = process_token(PostfixRegEx, [], dict:new()),
% 	io:format("~p~n", [Auto]).


% compile_and_print_both(InfixRegEx) ->
% 	io:format("before optimization:~n"),
% 	compile_and_print_old(InfixRegEx),
% 	io:format("after optimization:~n"),
% 	compile_and_print(InfixRegEx).

