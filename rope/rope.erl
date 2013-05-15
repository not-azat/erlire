%%% туду 
%%% 1. переделать все регулярки на бинари вместо строк
%%% 2. Воткнуть сюда re_transition
%%% 3. Тест ire


-module(rope).
-export([split/2, merge/2, empty_rope/0, binary2rope/1, rope2binary/1, length/1]).


-define(Leaf_size, 5). % leaf length can be no more than (2 * ?Leaf_size - 1)




-type key() :: term().
-type value() :: binary().

% we can not tag them because they are all of different lengths

-type rope_node() :: 
				{Key1::key(), 
					Key2::key(), 
					Tree1::not_empty_rope(), 
					Tree2::not_empty_rope(), 
					Tree3::not_empty_rope(),
					TransDict::cache()}
			  | {Key::key(), 
			  		Tree1::not_empty_rope(), 
			  		Tree2::not_empty_rope(),
			  		TransDict::cache()}
			  | {value(), TransDict::cache()}.

-type not_empty_rope() :: {Height::integer(), Length::key(), rope_node()}.

-type rope() :: empty_rope | not_empty_rope().

% cache of transition function value for sub-rope
-type cache() :: term().




-spec split(rope(), Position::key()) -> {Rope1::rope(), Rope2::rope()}.
%% splits Tree into two trees,
%% merge(Tree1, Tree2) == Tree,
%% length of Tree1 == Position.

% to be consistent
split(empty_rope, _) ->
	{empty_rope, empty_rope};
% miss
split(Tree, Pos) when Pos =< 0 -> 
	{empty_rope, Tree};
split(Tree = {_, Len, _}, Pos) when Pos >= Len -> 
	{Tree, empty_rope};
% 2-node
split({_, _, {Key, Tree1, Tree2, _}}, Pos) when Pos < Key -> 
	{Tree11, Tree12} = split(Tree1, Pos),
	{Tree11, merge(Tree12, Tree2)};
split({_, _, {Key, Tree1, Tree2, _}}, Pos) when Pos >= Key -> 
	{Tree21, Tree22} = split(Tree2, Pos - Key),
	{merge(Tree1, Tree21), Tree22};
% 3-node
split({_, _, {Key1, _, Tree1, Tree2, Tree3, _}}, Pos) when Pos < Key1 -> 
	{Tree11, Tree12} = split(Tree1, Pos),
	{Tree11, merge(Tree12, merge(Tree2, Tree3))};
split({_, _, {Key1, Key2, Tree1, Tree2, Tree3, _}}, Pos) when Pos < Key2 -> 
	{Tree21, Tree22} = split(Tree2, Pos - Key1),
	{merge(Tree1, Tree21), merge(Tree22, Tree3)};
split({_, _, {_, Key2, Tree1, Tree2, Tree3, _}}, Pos) when Pos >= Key2 -> 
	{Tree31, Tree32} = split(Tree3, Pos - Key2),
	{merge(Tree1, merge(Tree2, Tree31)), Tree32};
% leaf
split({0, Len, Leaf}, Pos) ->
	{Val, _Cache} = Leaf,
	<<Val1:Pos/binary, Val2/binary>> = Val,
	{
		{0, Pos, 
			{Val1, compute_cache(Val1)}}, 
		{0, Len - Pos, 
			{Val2, compute_cache(Val2)}}
	}.




-spec merge(rope(), rope()) -> rope().

% empty
merge(empty_rope, Tree2) -> Tree2;
merge(Tree1, empty_rope) -> Tree1;

% two leafs
merge({0, Len1, Leaf1}, {0, Len2, Leaf2}) ->
	{Val1, Cache1} = Leaf1,
	{Val2, Cache2} = Leaf2,
	case (Len1 + Len2 < 2 * ?Leaf_size) of 
		true ->
			NewVal = {<<Val1/binary, Val2/binary>>, merge_caches(Cache1, Cache2)},
			{0, Len1 + Len2, NewVal}; % tree with leaf
		false ->
			<<NewVal1:?Leaf_size/binary, NewVal2/binary>> = <<Val1/binary, Val2/binary>>,
			{1, Len1 + Len2, % tree
				{?Leaf_size + 1, % 2-node
					{0, ?Leaf_size, {NewVal1, compute_cache(NewVal1)}}, % tree with leaf
					{0, Len1 + Len2 - ?Leaf_size, {NewVal2, compute_cache(NewVal2)}}, % tree with leaf
					merge_caches(Cache1, Cache2)}} 
	end;

% equal height
merge(Tree1 = {H1, Len1, Node1}, Tree2 = {H2, Len2, Node2}) when H1 == H2 ->
	Cache1 = extract_tree_cache(Tree1), % ugly, use records
	Cache2 = extract_tree_cache(Tree2),
	{H1 + 1, Len1 + Len2, {Len1, {H1, Len1, Node1}, {H2, Len2, Node2}, merge_caches(Cache1, Cache2)}};

% diff == 1
% 2-node + node = 3-node
merge({H1, Len1, {Key1, Tree11, Tree12, Cache1}}, Tree2 = {H2, Len2, Node2}) when H1 == H2 + 1 ->
	Cache2 = extract_tree_cache(Tree2),
	{H1, Len1 + Len2, {Key1, Len1, Tree11, Tree12, {H2, Len2, Node2}, merge_caches(Cache1, Cache2)}};
% node + 2-node = 3-node
merge(Tree1 = {H1, Len1, Node1}, {H2, Len2, {Key2, Tree21, Tree22, Cache2}}) when H1 + 1 == H2 ->
	Cache1 = extract_tree_cache(Tree1),
	{H2, Len1 + Len2, {Len1, Len1 + Key2, {H1, Len1, Node1}, Tree21, Tree22, merge_caches(Cache1, Cache2)}};
% 3-node + small
merge({H1, Len1, {_, _, Tree11, Tree12, Tree13, Cache1}}, Tree2 = {H2, Len2, _}) when H1 == H2 + 1 ->
	Cache11 = extract_tree_cache(Tree11),
	Cache12 = extract_tree_cache(Tree12),
	Cache13 = extract_tree_cache(Tree13),
	Cache2 = extract_tree_cache(Tree2),
	{_, Len11, _} = Tree11,
	{_, Len12, _} = Tree12,
	{_, Len13, _} = Tree13,
	{H1 + 1, Len1 + Len2, % tree
		{Len11 + Len12, % 2-node
			{H1, Len11 + Len12, {Len11, Tree11, Tree12, merge_caches(Cache11, Cache12)}}, % tree with 2-node
			{H1, Len13 + Len2, {Len1, Tree13, Tree2, merge_caches(Cache13, Cache2)}}, % tree with 2-node
			merge_caches(Cache1, Cache2)}};
% small + 3-node
merge(Tree1 = {H1, Len1, _}, {H2, Len2, {_, _, Tree21, Tree22, Tree23, Cache2}}) when H2 == H1 + 1 ->
	Cache1 = extract_tree_cache(Tree1),
	Cache21 = extract_tree_cache(Tree21),
	Cache22 = extract_tree_cache(Tree22),
	Cache23 = extract_tree_cache(Tree23),
	{_, Len21, _} = Tree21,
	{_, Len22, _} = Tree22,
	{_, Len23, _} = Tree23,
	{H2 + 1, Len1 + Len2, % tree
		{Len1 + Len21, % 2-node
			{H2, Len1 + Len21, {Len1, Tree1, Tree21, merge_caches(Cache1, Cache21)}}, % tree with 2-node
			{H2, Len22 + Len23, {Len22, Tree22, Tree23, merge_caches(Cache22, Cache23)}},
			merge_caches(Cache1, Cache2)}}; % tree with 2-node

% diff > 1
% 2-node + small
merge({H1, _, {_, Tree11, Tree12, _}}, {H2, Len2, Node2}) when H1 > H2 + 1 ->
	merge(
		Tree11,
		merge(Tree12, {H2, Len2, Node2}));
% small + 2-node
merge({H1, Len1, Node1}, {H2, _, {_, Tree21, Tree22, _}}) when H2 > H1 + 1 ->
	merge(
		merge({H1, Len1, Node1}, Tree21),
		Tree22);
% 3-node + small
merge({H1, _, {_, _, Tree11, Tree12, Tree13, _}}, Tree2 = {H2, _, _}) when H1 > H2 + 1 ->
	merge(
		merge(Tree11, Tree12),
		merge(Tree13, Tree2));
% small + 3-node
merge(Tree1 = {H1, _, _}, {H2, _, {_, _, Tree21, Tree22, Tree23, _}}) when H2 > H1 + 1 ->
	merge(
		merge(Tree1, Tree21),
		merge(Tree22, Tree23)).




%% to start with
-spec empty_rope() -> rope().

empty_rope() -> empty_rope.




-spec binary2rope(binary()) -> rope().

binary2rope(Binary) ->
	binary2rope(Binary, empty_rope).

binary2rope(<<>>, AccRope) ->
	AccRope;
binary2rope(Binary, AccRope) when byte_size(Binary) =< ?Leaf_size ->
	Leaf = {Binary, compute_cache(Binary)},
	merge(AccRope, {0, byte_size(Binary), Leaf});
binary2rope(Binary, AccRope) ->
	<<LeftChunk:?Leaf_size/binary, RestBinary/binary>> = Binary,
	Leaf = {LeftChunk, compute_cache(LeftChunk)},
	binary2rope(RestBinary, merge(AccRope, {0, ?Leaf_size, Leaf})).




-spec rope2binary(rope()) -> binary().

rope2binary(empty_rope) ->
	<<>>;
rope2binary(Tree) ->
	rope2binary(Tree, <<>>).

rope2binary(empty_rope, AccBinary) ->
	AccBinary;
rope2binary({0, _, {Val, _}}, AccBinary) ->
	<<AccBinary/binary, Val/binary>>;
rope2binary({_, _, {_, Tree1, Tree2, _}}, AccBinary) ->
	LeftBinary = rope2binary(Tree1, AccBinary),
	rope2binary(Tree2, LeftBinary);
rope2binary({_, _, {_, _, Tree1, Tree2, Tree3, _}}, AccBinary) ->
	LeftBinary = rope2binary(Tree1, AccBinary),
	MiddleBinary = rope2binary(Tree2, LeftBinary),
	rope2binary(Tree3, MiddleBinary).




-spec length(rope()) -> non_neg_integer().

length(empty_rope) -> 0;
length({_, Len, _}) -> Len.	





compute_cache(Binary) -> dummy_cache.

merge_caches(Cache1, Cache2) -> dummy_cache.

extract_cache({_, _, _, _, _, Cache}) -> Cache;
extract_cache({_, _, _, Cache}) -> Cache;
extract_cache({_, Cache}) -> Cache.

extract_tree_cache({_, _, Node}) -> extract_cache(Node).


