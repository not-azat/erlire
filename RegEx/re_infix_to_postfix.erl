%%% Converting infix-form RegEx to postfix-form

-module(re_infix_to_postfix).
-export([convert/1]).
-include("re_types.hrl").


-spec convert(string()) -> string().

convert(RegEx) ->
	RegEx1 = insert_concat_op(RegEx),
	parse_token(RegEx1 ++ [re_end], [], []).



-spec insert_concat_op(string()) -> list().

insert_concat_op(RegEx) ->
	insert_concat_op(RegEx, re_begin, "").

insert_concat_op([], _, Result) ->
	lists:reverse(Result);
insert_concat_op([Token | Rest], PrevToken, Result) ->
	case (re_lang:is_concat(PrevToken, Token)) of 
		true -> 
			insert_concat_op(Rest, Token, [Token, $. | Result]);
		false -> 
			insert_concat_op(Rest, Token, [Token | Result])
	end.



-spec parse_token(RegEx::string(), OpStack::list(), ResultQueue::list()) -> PostfixRegEx::string().

parse_token([], [re_end], ResultQueue) ->
	lists:reverse(ResultQueue);
parse_token([], _OpStack, _ResultQueue) ->
	badarg;
parse_token([Token | Rest], OpStack, ResultQueue) ->
	case (re_lang:is_operator(Token)) of
		false -> 
			parse_token(Rest, OpStack, [Token | ResultQueue]);
		true -> 
			parse_operator(Token, Rest, OpStack, ResultQueue)
	end.


parse_operator(Op, RegExRest, [], ResultQueue) ->
	parse_token(RegExRest, [Op], ResultQueue);
parse_operator(Op, RegExRest, OpStack, ResultQueue) ->
	case (push_operator(Op, pop_operators(Op, OpStack, ResultQueue))) of 
		{NewOpStack, NewResultQueue} -> 
			parse_token(RegExRest, NewOpStack, NewResultQueue);
		badarg -> 
			badarg
	end.


pop_operators(_, [], ResultQueue) ->
	{[], ResultQueue};
pop_operators(_, [$( | RestOp], ResultQueue) ->
	{[$( | RestOp], ResultQueue};
pop_operators(Op, OpStack = [LastOp | RestOp], ResultQueue) ->
	case (re_lang:priority(Op) =< re_lang:priority(LastOp)) of
		true -> 
			pop_operators(Op, RestOp, [LastOp | ResultQueue]);
		false -> 
			{OpStack, ResultQueue}
	end.


push_operator(_, {[$) | _OpStackRest], _ResultQueue}) ->
	badarg;
push_operator($), {[$( | OpStackRest], ResultQueue}) ->
	{OpStackRest, ResultQueue};
push_operator(Op, {OpStack, ResultQueue}) ->
	{[Op | OpStack], ResultQueue}.

