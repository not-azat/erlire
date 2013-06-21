-module(meta_ire).
-export([new/3, matches/1, length/1]).

-behaviour(gen_rope).
-export([length/2, cache/2, merge_values/3, split_value/3, merge_caches/3]).


-define(LEAF_LEN, 65).

-type meta_ire() :: {meta_ire,
					 gen_rope:rope(),
					 re_transition:transition_automata(),
					 LeafLength::number()}.

-type server_leaf() :: {pid(), Length::number()}.

% public
-spec new(binary(), re_transition:transition_automata(), LeafLength::number()) -> meta_ire().

new(BinString, TransAutomata, LeafLength) ->
	{meta_ire,
		binary2metarope(BinString, {?MODULE, LeafLength, TransAutomata}),
		TransAutomata,
		LeafLength}.


% % public
% -spec merge(ire(), ire()) -> ire().

% merge({ire, Rope1, TransAutomata, LeafLength}, {ire, Rope2, _, _}) ->
% 	{ire,
% 		gen_rope:merge(Rope1, Rope2, {?MODULE, LeafLength, TransAutomata}),
% 		TransAutomata,
% 		LeafLength}.


% % public
% -spec split(ire(), number()) -> {ire(), ire()}.

% split({ire, Rope, TransAutomata, LeafLength}, Pos) ->
% 	{Rope1, Rope2} = gen_rope:split(Rope, Pos, {?MODULE, LeafLength, TransAutomata}),
% 	{
% 		{ire, Rope1, TransAutomata, LeafLength},
% 		{ire, Rope2, TransAutomata, LeafLength}
% 	}.


% public
-spec matches(meta_ire()) -> boolean().

matches({meta_ire, Rope, _, _}) ->
	case (gen_rope:get_cache(Rope)) of 
		{Matches, _} -> Matches;
		undefined -> false
	end.


% % public
% -spec get_transition(ire()) -> re_transition:transition() | undefined.

% get_transition({ire, Rope, _, _}) ->
% 	case (gen_rope:get_cache(Rope)) of 
% 		{_, Transition} -> Transition;
% 		undefined -> undefined
% 	end.


% public
-spec length(meta_ire()) -> integer().

length({meta_ire, Rope, _, _}) ->
	gen_rope:length(Rope).


-spec binary2metarope(binary(), gen_rope:params()) -> gen_rope:rope().

binary2metarope(Binary, Params) ->
	{_, LeafLength, TransAutomata} = Params,
	% io:format("binary2metarope (third should be TransAutomata) Params: ~n~p~n", [Params]),
	PidList = spawn_leaf_servers(Binary, TransAutomata, LeafLength, []),
	build_rope(PidList, TransAutomata).


-spec spawn_leaf_servers(
		binary(), 
		re_transition:transition_automata(), 
		number(), 
		[server_leaf()]) 
	-> [server_leaf()].

spawn_leaf_servers(Binary, TransAutomata, LeafLength, AccPidList) when byte_size(Binary) =< LeafLength ->
	Leaf = {leaf_server:start(Binary, TransAutomata, ?LEAF_LEN), byte_size(Binary)},
	[Leaf | AccPidList];
spawn_leaf_servers(Binary, TransAutomata, LeafLength, AccPidList) ->
	%io:format("spawn_leaf_servers: ~n~p~n", [Binary]),
	<<LeafBin:LeafLength/binary, RestBin/binary>> = Binary,
	Leaf = {leaf_server:start(LeafBin, TransAutomata, ?LEAF_LEN), byte_size(LeafBin)},
	spawn_leaf_servers(
		RestBin, 
		TransAutomata,
		LeafLength, 
		[Leaf | AccPidList]).


build_rope(PidList, TransAutomata) ->
	build_rope(PidList, TransAutomata, nothing).

build_rope([], _, nothing) ->
	throw('no leafs to build rope');
build_rope([], _, AccRope) ->
	AccRope;
build_rope([{Pid, Len} | Rest], TransAutomata, nothing) ->
	Rope = gen_rope:leaf_rope({Pid, Len}, {?MODULE, ?LEAF_LEN, TransAutomata}),
	build_rope(Rest, TransAutomata, Rope);
build_rope([{Pid, Len} | Rest], TransAutomata, AccRope) ->
	Rope = gen_rope:leaf_rope({Pid, Len}, {?MODULE, ?LEAF_LEN, TransAutomata}),
	AccRope1 = gen_rope:merge(AccRope, Rope, {?MODULE, ?LEAF_LEN, TransAutomata}),
	build_rope(Rest, TransAutomata, AccRope1).



%%% Callbacks for gen_ire %%%

-spec length({pid(), number()}, term()) -> number().

length({_, Len}, _) ->
	Len.


-spec cache(server_leaf(), term()) -> {boolean(), re_transition:transition()}.

cache({Pid, _}, TransAutomata) ->
	% io:format("meta ire: cache~n"),
	Transition = leaf_server:get_transition(Pid),
	{re_transition:matches(Transition, TransAutomata), Transition}.


% length(Value1) + length(Value2) = length(merge_values(Value1, Value2))
-spec merge_values(server_leaf(), server_leaf(), term()) -> server_leaf().

merge_values({Pid1, Len1}, {Pid2, Len2}, TransAutomata) ->
	% io:format("merge_values: ~n~p~n~p~n", [{Pid1, Len1}, {Pid2, Len2}]),
	Ire1 = leaf_server:get_ire(Pid1),
	Ire2 = leaf_server:get_ire(Pid2),
	leaf_server:dispose(Pid1),
	leaf_server:dispose(Pid2),
	Ire = ire:merge(Ire1, Ire2),
	Pid = leaf_server:start(Ire, TransAutomata),
	{Pid, Len1 + Len2}.


% if {Value1, Value2} = split_value(Value) 
% then length(Value1) + length(Value2) = length(Value)
-spec split_value(server_leaf(), number(), term()) -> {server_leaf(), server_leaf()}.

split_value({Pid, Len}, N, TransAutomata) ->
	% io:format("split_value: ~n~p~n~p~n", [{Pid, Len}, N]),
	Ire = leaf_server:get_ire(Pid),
	{Ire1, Ire2} = ire:split(Ire, N),
	leaf_server:dispose(Pid),
	Pid1 = leaf_server:start(Ire1, TransAutomata),
	Pid2 = leaf_server:start(Ire2, TransAutomata),
	{{Pid1, N}, {Pid2, Len - N}}.


-spec merge_caches(
		{boolean(), re_transition:transition()}, 
		{boolean(), re_transition:transition()}, 
		term()) -> {boolean(), re_transition:transition()}.

merge_caches({Matches1, Transition1}, {Matches2, Transition2}, TransAutomata) ->
	% io:format("ire: merge_caches~n"),
	Transition = re_transition:compose_transitions(Transition1, Transition2),
	Matches = re_transition:matches(Transition, TransAutomata),
	{Matches or Matches1 or Matches2, Transition}.
