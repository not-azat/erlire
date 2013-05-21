-module(ire).
-export([new/3, merge/2, split/2, matches/1, length/1]).

-behaviour(gen_rope).
-export([length/2, cache/2, merge_values/3, split_value/3, merge_caches/3]).


-type ire() :: {ire, 
				gen_rope:rope(), 
				re_transition:transition_automata(), 
				LeafLength::number()}.


% public
-spec new(binary(), re_transition:transition_automata(), LeafLength::number()) -> ire().

new(BinString, TransAutomata, LeafLength) ->
	{ire,
		binary2rope(BinString, {?MODULE, LeafLength, TransAutomata}),
		TransAutomata,
		LeafLength}.


% public
-spec merge(ire(), ire()) -> ire().

merge({ire, Rope1, TransAutomata, LeafLength}, {ire, Rope2, _, _}) ->
	{ire,
		gen_rope:merge(Rope1, Rope2, {?MODULE, LeafLength, TransAutomata}),
		TransAutomata,
		LeafLength}.


% public
-spec split(ire(), number()) -> {ire(), ire()}.

split({ire, Rope, TransAutomata, LeafLength}, Pos) ->
	{Rope1, Rope2} = gen_rope:split(Rope, Pos, {?MODULE, LeafLength, TransAutomata}),
	{
		{ire, Rope1, TransAutomata, LeafLength},
		{ire, Rope2, TransAutomata, LeafLength}
	}.


% public
-spec matches(ire()) -> boolean().

matches({ire, Rope, _, _}) ->
	{Matches, _} = gen_rope:get_cache(Rope),
	Matches.


% public
-spec length(ire()) -> integer().

length({ire, Rope, _, _}) ->
	gen_rope:length(Rope).


-spec binary2rope(binary(), gen_rope:params()) -> gen_rope:rope().

binary2rope(<<>>, Params) ->
	gen_rope:leaf_rope(<<>>, Params);
binary2rope(Binary, Params) ->
	binary2rope(Binary, nothing, Params).

binary2rope(<<>>, AccRope, _Params) ->
	AccRope; % never 'nothing'
binary2rope(Binary, AccRope, Params) ->
	{_, LeafLength, _} = Params,
	case {byte_size(Binary) =< LeafLength, AccRope == nothing} of 
		{true, false} ->
			NewRope = gen_rope:leaf_rope(Binary, Params),
			gen_rope:merge(AccRope, NewRope, Params);
		{true, true} ->
			gen_rope:leaf_rope(Binary, Params);
		{false, false} ->
			<<LeftChunk:LeafLength/binary, RestBinary/binary>> = Binary,
			NewRope = gen_rope:leaf_rope(LeftChunk, Params),
			binary2rope(RestBinary, gen_rope:merge(AccRope, NewRope, Params), Params);
		{false, true} ->
			<<LeftChunk:LeafLength/binary, RestBinary/binary>> = Binary,
			binary2rope(RestBinary, gen_rope:leaf_rope(LeftChunk, Params), Params)
	end.



%%% Callbacks for gen_ire %%%

-spec length(binary(), term()) -> number().

length(BinString, _) ->
	byte_size(BinString).


-spec cache(binary(), term()) -> {boolean(), re_transition:transition()}.

cache(BinString, TransAutomata) ->
	io:format("ire: cache~n"),
	Transition = re_transition:find_transition(BinString, TransAutomata),
	{re_transition:matches(Transition, TransAutomata), Transition}.


% length(Value1) + length(Value2) = length(merge_values(Value1, Value2))
-spec merge_values(binary(), binary(), term()) -> binary().

merge_values(BinString1, BinString2, _) ->
	<<BinString1/binary, BinString2/binary>>.


% if {Value1, Value2} = split_value(Value) 
% then length(Value1) + length(Value2) = length(Value)
-spec split_value(binary(), number(), term()) -> {binary(), binary()}.

split_value(BinString, Pos, _) ->
	<<BinString1:Pos/binary, BinString2/binary>> = BinString,
	{BinString1, BinString2}.


-spec merge_caches(
		{boolean(), re_transition:transition()}, 
		{boolean(), re_transition:transition()}, 
		term()) -> {boolean(), re_transition:transition()}.

merge_caches({Matches1, Transition1}, {Matches2, Transition2}, TransAutomata) ->
	io:format("ire: merge_caches~n"),
	Transition = re_transition:compose_transitions(Transition1, Transition2),
	Matches = re_transition:matches(Transition, TransAutomata),
	{Matches or Matches1 or Matches2, Transition}.
