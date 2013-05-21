-module(gen_rope).
-export([leaf_rope/2, get_cache/1, split/3, merge/3, flatten/2, length/1]).


-type key() :: number().
-type value() :: term().

% some cache for every rope
-type cache() :: term().

% params used to generalize behaviour
% leaf length can be no more than (2 * LeafLength - 1)
-type params() :: {Module::atom(), LeafLength::number(), term()}.

-type rope_node() :: 
				{Key1::key(), Key2::key(), 
					Tree1::not_empty_rope(), 
					Tree2::not_empty_rope(), 
					Tree3::not_empty_rope(),
					cache()}
			  | {Key::key(), 
			  		Tree1::not_empty_rope(), 
			  		Tree2::not_empty_rope(),
			  		cache()}
			  | {value(), cache()}.

-type not_empty_rope() :: {Height::integer(), Length::key(), rope_node()}.

-type rope() :: empty_rope | not_empty_rope().


-callback length(value(), term()) -> number().
-callback cache(value(), term()) -> cache().
% length(Value1) + length(Value2) = length(merge_values(Value1, Value2))
-callback merge_values(value(), value(), term()) -> value().
% if {Value1, Value2} = split_value(Value) 
% then length(Value1) + length(Value2) = length(Value)
-callback split_value(value(), number(), term()) -> {value(), value()}.
-callback merge_caches(cache(), cache(), term()) -> cache().


% public
-spec leaf_rope(value(), params()) -> rope().

leaf_rope(Value, {Module, _, GiveBack}) ->
	Len = Module:length(Value, GiveBack),
	Cache = Module:cache(Value, GiveBack),
	{0, Len, {Value, Cache}}.


% public
-spec get_cache(rope()) -> cache() | undefined.

get_cache(empty_rope) -> undefined;
get_cache({_, _, {_, Cache}}) -> Cache;
get_cache({_, _, {_, _, _, Cache}}) -> Cache;
get_cache({_, _, {_, _, _, _, _, Cache}}) -> Cache.


% public
-spec split(rope(), Position::key(), params()) -> {Rope1::rope(), Rope2::rope()}.
%% splits Tree into two trees,
%% merge(Tree1, Tree2) == Tree,
%% length of Tree1 == Position.

% to be consistent
split(empty_rope, _, _) ->
	{empty_rope, empty_rope};
% miss
split(Tree, Pos, _) when Pos =< 0 -> 
	{empty_rope, Tree};
split(Tree = {_, Len, _}, Pos, _) when Pos >= Len -> 
	{Tree, empty_rope};
% 2-node
split({_, _, {Key, Tree1, Tree2, _}}, Pos, Params) when Pos < Key -> 
	{Tree11, Tree12} = split(Tree1, Pos, Params),
	{Tree11, merge(Tree12, Tree2, Params)};
split({_, _, {Key, Tree1, Tree2, _}}, Pos, Params) when Pos >= Key -> 
	{Tree21, Tree22} = split(Tree2, Pos - Key, Params),
	{merge(Tree1, Tree21, Params), Tree22};
% 3-node
split({_, _, {Key1, _, Tree1, Tree2, Tree3, _}}, Pos, Params) when Pos < Key1 -> 
	{Tree11, Tree12} = split(Tree1, Pos, Params),
	{Tree11, merge(Tree12, merge(Tree2, Tree3, Params), Params)};
split({_, _, {Key1, Key2, Tree1, Tree2, Tree3, _}}, Pos, Params) when Pos < Key2 -> 
	{Tree21, Tree22} = split(Tree2, Pos - Key1, Params),
	{merge(Tree1, Tree21, Params), merge(Tree22, Tree3, Params)};
split({_, _, {_, Key2, Tree1, Tree2, Tree3, _}}, Pos, Params) when Pos >= Key2 -> 
	{Tree31, Tree32} = split(Tree3, Pos - Key2, Params),
	{merge(Tree1, merge(Tree2, Tree31, Params), Params), Tree32};
% leaf
split({0, Len, Leaf}, Pos, Params) ->
	{Val, _} = Leaf,
	{Module, _, GiveBack} = Params,
	{Val1, Val2} = Module:split_value(Val, Pos, GiveBack),
	Leaf1 = {Val1, Module:cache(Val1, GiveBack)},
	Leaf2 = {Val2, Module:cache(Val2, GiveBack)},
	{{0, Pos, Leaf1}, {0, Len - Pos, Leaf2}}.




-spec merge(rope(), rope(), params()) -> rope().

% empty
merge(empty_rope, Tree2, _) -> Tree2;
merge(Tree1, empty_rope, _) -> Tree1;

% two leafs
merge({0, Len1, Leaf1}, {0, Len2, Leaf2}, Params) ->
	{Val1, Cache1} = Leaf1,
	{Val2, Cache2} = Leaf2,
	{Module, LeafLength, GiveBack} = Params,
	case (Len1 + Len2 < 2 * LeafLength) of 
		true ->
			Leaf = {Module:merge_values(Val1, Val2, GiveBack), Module:merge_caches(Cache1, Cache2, GiveBack)},
			{0, Len1 + Len2, Leaf}; % tree with leaf
		false ->
			{NewVal1, NewVal2} = Module:split_value(Module:merge_values(Val1, Val2, GiveBack), LeafLength, GiveBack),
			NewCache1 = Module:cache(NewVal1, GiveBack),
			NewCache2 = Module:cache(NewVal2, GiveBack),
			NewLen1 = Module:length(NewVal1, GiveBack),
			NewLen2 = Module:length(NewVal2, GiveBack),
			{1, NewLen1 + NewLen2, % tree
				{NewLen1 + 1, % 2-node
					{0, NewLen1, {NewVal1, NewCache1}}, % tree with leaf
					{0, NewLen2, {NewVal2, NewCache2}}, % tree with leaf
					Module:merge_caches(NewCache1, NewCache2, GiveBack)}} 
	end;

% equal height
merge(Tree1 = {H1, Len1, Node1}, Tree2 = {H2, Len2, Node2}, Params) when H1 == H2 ->
	Cache1 = extract_tree_cache(Tree1),
	Cache2 = extract_tree_cache(Tree2),
	{Module, _, GiveBack} = Params,
	{H1 + 1, Len1 + Len2, {Len1, {H1, Len1, Node1}, {H2, Len2, Node2}, Module:merge_caches(Cache1, Cache2, GiveBack)}};

% diff == 1
% 2-node + node = 3-node
merge({H1, Len1, {Key1, Tree11, Tree12, Cache1}}, Tree2 = {H2, Len2, Node2}, Params) when H1 == H2 + 1 ->
	Cache2 = extract_tree_cache(Tree2),
	{Module, _, GiveBack} = Params,
	{H1, Len1 + Len2, {Key1, Len1, Tree11, Tree12, {H2, Len2, Node2}, Module:merge_caches(Cache1, Cache2, GiveBack)}};
% node + 2-node = 3-node
merge(Tree1 = {H1, Len1, Node1}, {H2, Len2, {Key2, Tree21, Tree22, Cache2}}, Params) when H1 + 1 == H2 ->
	Cache1 = extract_tree_cache(Tree1),
	{Module, _, GiveBack} = Params,
	{H2, Len1 + Len2, {Len1, Len1 + Key2, {H1, Len1, Node1}, Tree21, Tree22, Module:merge_caches(Cache1, Cache2, GiveBack)}};
% 3-node + small
merge({H1, Len1, {_, _, Tree11, Tree12, Tree13, Cache1}}, Tree2 = {H2, Len2, _}, Params) when H1 == H2 + 1 ->
	Cache11 = extract_tree_cache(Tree11),
	Cache12 = extract_tree_cache(Tree12),
	Cache13 = extract_tree_cache(Tree13),
	Cache2 = extract_tree_cache(Tree2),
	{Module, _, GiveBack} = Params,
	{_, Len11, _} = Tree11,
	{_, Len12, _} = Tree12,
	{_, Len13, _} = Tree13,
	{H1 + 1, Len1 + Len2, % tree
		{Len11 + Len12, % 2-node
			{H1, Len11 + Len12, {Len11, Tree11, Tree12, Module:merge_caches(Cache11, Cache12, GiveBack)}}, % tree with 2-node
			{H1, Len13 + Len2, {Len1, Tree13, Tree2, Module:merge_caches(Cache13, Cache2, GiveBack)}}, % tree with 2-node
			Module:merge_caches(Cache1, Cache2, GiveBack)}};
% small + 3-node
merge(Tree1 = {H1, Len1, _}, {H2, Len2, {_, _, Tree21, Tree22, Tree23, Cache2}}, Params) when H2 == H1 + 1 ->
	Cache1 = extract_tree_cache(Tree1),
	Cache21 = extract_tree_cache(Tree21),
	Cache22 = extract_tree_cache(Tree22),
	Cache23 = extract_tree_cache(Tree23),
	{Module, _, GiveBack} = Params,
	{_, Len21, _} = Tree21,
	{_, Len22, _} = Tree22,
	{_, Len23, _} = Tree23,
	{H2 + 1, Len1 + Len2, % tree
		{Len1 + Len21, % 2-node
			{H2, Len1 + Len21, {Len1, Tree1, Tree21, Module:merge_caches(Cache1, Cache21, GiveBack)}}, % tree with 2-node
			{H2, Len22 + Len23, {Len22, Tree22, Tree23, Module:merge_caches(Cache22, Cache23, GiveBack)}},
			Module:merge_caches(Cache1, Cache2, GiveBack)}}; % tree with 2-node

% diff > 1
% 2-node + small
merge({H1, _, {_, Tree11, Tree12, _}}, {H2, Len2, Node2}, Params) when H1 > H2 + 1 ->
	merge(
		Tree11,
		merge(Tree12, {H2, Len2, Node2}, Params), 
		Params);
% small + 2-node
merge({H1, Len1, Node1}, {H2, _, {_, Tree21, Tree22, _}}, Params) when H2 > H1 + 1 ->
	merge(
		merge({H1, Len1, Node1}, Tree21, Params),
		Tree22,
		Params);
% 3-node + small
merge({H1, _, {_, _, Tree11, Tree12, Tree13, _}}, Tree2 = {H2, _, _}, Params) when H1 > H2 + 1 ->
	merge(
		merge(Tree11, Tree12, Params),
		merge(Tree13, Tree2, Params),
		Params);
% small + 3-node
merge(Tree1 = {H1, _, _}, {H2, _, {_, _, Tree21, Tree22, Tree23, _}}, Params) when H2 > H1 + 1 ->
	merge(
		merge(Tree1, Tree21, Params),
		merge(Tree22, Tree23, Params),
		Params).


% % public
-spec flatten(rope(), params()) -> value() | undefined.

flatten(empty_rope, _) ->
	undefined;
flatten(Tree, Params) ->
	flatten(Tree, undefined, Params).

flatten(empty_rope, AccBinary, _) ->
	AccBinary;
flatten({0, _, {Val, _}}, undefined, _) ->
	Val;
flatten({0, _, {Val, _}}, AccBinary, {Module, _, GiveBack}) ->
	Module:merge_values(AccBinary, Val, GiveBack);
flatten({_, _, {_, Tree1, Tree2, _}}, AccBinary, Params) ->
	LeftBinary = flatten(Tree1, AccBinary, Params),
	flatten(Tree2, LeftBinary, Params);
flatten({_, _, {_, _, Tree1, Tree2, Tree3, _}}, AccBinary, Params) ->
	LeftBinary = flatten(Tree1, AccBinary, Params),
	MiddleBinary = flatten(Tree2, LeftBinary, Params),
	flatten(Tree3, MiddleBinary, Params).


% public
-spec length(rope()) -> non_neg_integer().

length(empty_rope) -> 0;
length({_, Len, _}) -> Len.	


extract_cache({_, _, _, _, _, Cache}) -> Cache;
extract_cache({_, _, _, Cache}) -> Cache;
extract_cache({_, Cache}) -> Cache.

extract_tree_cache({_, _, Node}) -> extract_cache(Node).


