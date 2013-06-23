-module(utils).
-export([swap_first_and_second_key/1]).


-spec swap_first_and_second_key(dict()) -> dict().
%% dict(First => dict(Second => Value))  
%% to
%% dict(Second => dict(First => Value))
swap_first_and_second_key(First2Second2Value) ->
	dict:fold(
		fun(First, Second2Value, Second2First2ValueAcc) ->
			SwappedPairsList = make_all_swapped_pairs(First, Second2Value), % [{Second, dict(First => Value)}]
			add_all_swapped_pairs(SwappedPairsList, Second2First2ValueAcc)
		end,
		dict:new(),
		First2Second2Value).


-spec make_all_swapped_pairs(
		First::term(), 
		Second2Value :: dict()) -> list({Second::term(), First::term(), Value::term()}).

make_all_swapped_pairs(First, Second2Value) ->
	dict:fold(
		fun(Second, Value, AccList) ->
			[{Second, First, Value} | AccList]
		end,
		[],
		Second2Value).


-spec add_all_swapped_pairs(
		SFVList::list({Second::term(), First::term(), Value::term()}), 
		Second2First2ValueAcc::dict()) -> dict().

add_all_swapped_pairs(SFVList, Second2First2ValueAcc) ->
	lists:foldl(
		fun({Second, First, Value}, InnerAccDict) ->
			case dict:find(Second, InnerAccDict) of 
				{ok, First2Value} ->
					First2Value1 = dict:store(First, Value, First2Value),
					dict:store(Second, First2Value1, InnerAccDict);
				error ->
					First2Value = dict:store(First, Value, dict:new()),
					dict:store(Second, First2Value, InnerAccDict)
			end
		end,
		Second2First2ValueAcc,
		SFVList).


