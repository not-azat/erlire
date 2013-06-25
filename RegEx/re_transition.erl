-module(re_transition).
-export([convert_automata/1, find_transition/2, compose/2, matches/2]).
-export([example/0, example2/0]).


-type re_automata() :: {re_automata, InitialExpNum::integer(), dict()}.
%% where dict(Index => {IsFinal::boolean(), dict(Letter => set(Index))})

-type transition_automata() :: {transition_automata, N::number(), InitialStates::bitstring(), FinalStates::bitstring(), list()}.
%% where dict(Letter => transition())

-type transition() :: {From::bitstring(), To::[bitstring()]}.
%% bitstrings of equal size. e.g., 
%% From = 10001000 and To = [01100000, 00100001] 
%% means that:
%% from node 0 there are links to nodes 1 and 2,
%% from node 4 there are links to nodes 2 and 7.


%%% Converting %%%

%% public
-spec convert_automata(re_automata()) -> transition_automata().

convert_automata({re_automata, InitialExpNum, AutomataDict}) ->
	% dict(Index => {IsFinal::boolean(), dict(Letter => set(Index))})
	AutomataDictNoIsFinal = dict:map(
		fun(_, {_, Value}) -> Value end,
		AutomataDict), 
	% dict(Index => dict(Letter => set(Index)))
	LetterToIndexToSet = utils:swap_first_and_second_key(AutomataDictNoIsFinal),
	N = 1 + find_largest_index(LetterToIndexToSet), % TODO slooow, maybe add N to re_automata()
	% dict(Letter => dict(Index => set(Index)))
	TransitionDict = dict:to_list(dict:map(
		fun(_, IndexToSet) ->
			compact_transition(IndexToSet, N)
		end,
		LetterToIndexToSet)),
	% dict(Letter => transition())
	InitialStates = bit:bitmask([InitialExpNum], N),
	FinalStatesList = lists:sort(dict:fold(
		fun
			(Index, {true, _}, AccList) -> [Index | AccList];
			(_, _, AccList) -> AccList
		end,
		[],
		AutomataDict)),
	FinalStates = bit:bitmask(FinalStatesList, N),
	{transition_automata, N, InitialStates, FinalStates, TransitionDict}.


find_largest_index(LetterToIndexToSet) ->
	L1 = dict:to_list(LetterToIndexToSet),
	L2 = lists:map(
		fun({_, IndexToSet}) -> dict:to_list(IndexToSet) end,
		L1),
	L3 = lists:flatten(L2),
	L4 = lists:map(
		fun({Index, Set}) ->
			[Index] ++ gb_sets:to_list(Set)
		end,
		L3),
	L5 = lists:flatten(L4),
	lists:foldl(
		fun(X, Acc) ->
			if (X > Acc) -> X; true -> Acc end
		end,
		0,
		L5).


compact_transition(IndexToSet, Largest) ->
	Indeces = lists:sort(dict:fetch_keys(IndexToSet)),
	Vs = lists:map(
		fun(Index) ->
			{ok, Set} = dict:find(Index, IndexToSet),
			Dests = lists:sort(gb_sets:to_list(Set)),
			bit:bitmask(Dests, Largest)
		end,
		Indeces),
	K = bit:bitmask(Indeces, Largest),
	{K, Vs}.


%%% Finding transition %%%

%% public
-spec find_transition(binary(), transition_automata()) -> transition().

find_transition(<<>>, {transition_automata, N, _, _, _}) ->
 	Full = bit:bitmask(one, N),
 	List = [ Full || _ <- lists:seq(1, N) ],
	{Full, List};
find_transition(<<Letter:1/binary, Rest/binary>>, {transition_automata, N, _, _, TransitionDict} = TAutomata) ->
	Transition = case (lists:keyfind(Letter, 1, TransitionDict)) of 
		{Letter, Trans} -> Trans;
		false -> {bit:bitmask(zero, N), []}
	end,
	case (Rest) of 
		<<>> -> Transition;
		_ -> compose(Transition, find_transition(Rest, TAutomata))
	end.


%%% Composing %%%

%% public
-spec compose(transition(), transition()) -> transition().

compose({K1, Vs1}, {K2, Vs2}) ->
	ToOrs = [ bit:bitand(V1, K2) || V1 <- Vs1 ],
	Empty = bit:bitmask(zero, bit_size(K1)),
	Composed = [ compose_masks(ToOr, K2, Vs2, Empty) || ToOr <- ToOrs ],
	filter_mask(K1, Composed, Empty).


-spec compose_masks(bitstring(), bitstring(), [bitstring()], bitstring()) -> bitstring().

compose_masks(<<>>, <<>>, [], Acc) ->
	Acc;
compose_masks(<<1:1, ToOrRest/bitstring>>, <<1:1, K2Rest/bitstring>>, [V2 | Vs2], Acc) ->
	compose_masks(ToOrRest, K2Rest, Vs2, bit:bitor(V2, Acc));
compose_masks(<<0:1, ToOrRest/bitstring>>, <<1:1, K2Rest/bitstring>>, [_ | Vs2], Acc) ->
	compose_masks(ToOrRest, K2Rest, Vs2, Acc);
compose_masks(<<0:1, ToOrRest/bitstring>>, <<0:1, K2Rest/bitstring>>, Vs2, Acc) ->
	compose_masks(ToOrRest, K2Rest, Vs2, Acc).


-spec filter_mask(bitstring(), [bitstring()], bitstring()) -> {bitstring(), [bitstring()]}.

filter_mask(K1, Composed, Empty) ->
		NewK = build_mask(K1, Composed, Empty, <<>>), 
		NewVs = [ Mask || Mask <- Composed, Mask /= Empty ],
		{NewK, NewVs}.


build_mask(<<>>, [], _, Acc) ->
	Acc;
build_mask(<<0:1, K1Rest/bitstring>>, L, Empty, Acc) ->
	build_mask(K1Rest, L, Empty, <<Acc/bitstring, 0:1>>);
build_mask(<<1:1, K1Rest/bitstring>>, [Empty | Rest], Empty, Acc) ->
	build_mask(K1Rest, Rest, Empty, <<Acc/bitstring, 0:1>>);
build_mask(<<1:1, K1Rest/bitstring>>, [_ | Rest], Empty, Acc) ->
	build_mask(K1Rest, Rest, Empty, <<Acc/bitstring, 1:1>>);
build_mask(_, _, _, _) ->
	throw('strange').


%%% Matching %%%

%% public
-spec matches(transition(), transition_automata()) -> boolean().

matches({K, Vs}, {transition_automata, N, InitialStates, FinalStates, _}) ->
	Empty = bit:bitmask(zero, N),
	HasInitial = bit:bitand(K, InitialStates) /= Empty,
	Finals = [ 0 || V <- Vs, bit:bitand(V, FinalStates) /= Empty ],
	HasInitial and (length(Finals) /= 0).


%%%%%% DEBUGGING %%%%%%

example() ->
	Automata = re_compiler:compile(<<"ab?cd">>),
	re_compiler:print_final_automata(Automata),
	TransitionAutomata = convert_automata(Automata),
	io:format("Transition for <<a>>:~n"),
	print_transition(find_transition(<<"a">>, TransitionAutomata)),
	ok.

example2() ->
	filter_mask(<<95,1:2>>, [<<0,1:2>>,<<0,1:2>>,<<0,1:2>>,<<0,1:2>>,<<0,1:2>>,<<0,1:2>>,<<0,1:2>>], <<0,0:2>>).

print_transition({K, Vs}) ->
	io:format("transition: from ~p to:~n", [bitstring2binary(K)]),
	[ io:format("~p~n", [bitstring2binary(V)]) || V <- Vs ],
	io:format("-----------~n"),
	ok.

print_transition_automata({transition_automata, N, InitialStates, FinalStates, TransitionDict}) ->
	io:format("transition_automata:~nN: ~p~ninitial states: ~p~nfinal states: ~p~n",
		[N, bitstring2binary(InitialStates), bitstring2binary(FinalStates)]),
	lists:map(
		fun({Letter, Transition}) ->
			io:format("~p => ~n", [Letter]),
			print_transition(Transition)
		end,
		TransitionDict),
	ok.

bitstring2binary(<<>>) ->
	<<>>;
bitstring2binary(<<X:1, Rest/bitstring>>) ->
	<<0:7, X:1, (bitstring2binary(Rest))/bitstring>>.


