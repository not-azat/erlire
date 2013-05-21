%%% Converting infix-form RegEx to postfix-form
-module(re_infix_to_postfix).
-export([convert/1]).
-compile(export_all).
-include("re_types.hrl").


-type postfix_regex_term() :: binary() | atom().
-type postfix_regex() :: list(postfix_regex_term()).


% public
-spec convert(string()) -> postfix_regex().

convert(RegEx) ->
	RegEx1 = escape(RegEx),
	RegEx2 = lists:map(fun(S) -> to_atoms(S) end, RegEx1),
	RegEx3 = lists:map(fun to_binaries/1, RegEx2),
	RegEx4 = insert_concat_op(RegEx3),
	parse_token(RegEx4 ++ ['end'], [], []).


-spec escape(string()) -> list().

escape(String) ->
	lists:reverse(escape(String, [])).

escape([], Result) ->
	Result;
escape([92, Next | Rest], Result) ->
	escape(Rest, [<<Next>> | Result]);
escape([92], _) ->
	throw('wrong escape symbol usage');
escape([Any | Rest], Result) ->
	escape(Rest, [Any | Result]).


-spec to_atoms(char()) -> atom() | char();
			  (binary()) -> binary().	

to_atoms($?) -> '?';
to_atoms($*) -> '*';
to_atoms($+) -> '+';
to_atoms($() -> '(';
to_atoms($)) -> ')';
to_atoms($|) -> '|';
to_atoms($.) -> any;
to_atoms(S) -> S.


-spec to_binaries(char()) -> binary();
				 (binary()) -> binary();
				 (atom()) -> atom().

to_binaries(S) when is_number(S) -> <<S>>;
to_binaries(S) -> S.


-spec insert_concat_op(string()) -> list().

insert_concat_op(RegEx) ->
	insert_concat_op(RegEx, 'begin', []).

insert_concat_op([], _, Result) ->
	lists:reverse(Result);
insert_concat_op([Token | Rest], PrevToken, Result) ->
	case (re_lang:is_concat(PrevToken, Token)) of 
		true -> 
			insert_concat_op(Rest, Token, [Token, 'concat' | Result]);
		false -> 
			insert_concat_op(Rest, Token, [Token | Result])
	end.


-spec parse_token(RegEx::string(), OpStack::list(), ResultQueue::list()) -> PostfixRegEx::string().

parse_token([], ['end'], ResultQueue) ->
	lists:reverse(ResultQueue);
parse_token([], _OpStack, _ResultQueue) ->
	throw('bad regex');
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
	{NewOpStack, NewResultQueue} = (push_operator(Op, pop_operators(Op, OpStack, ResultQueue))),
	parse_token(RegExRest, NewOpStack, NewResultQueue).


pop_operators(_, [], ResultQueue) ->
	{[], ResultQueue};
pop_operators(_, ['(' | RestOp], ResultQueue) ->
	{['(' | RestOp], ResultQueue};
pop_operators(Op, OpStack = [LastOp | RestOp], ResultQueue) ->
	case (re_lang:priority(Op) =< re_lang:priority(LastOp)) of
		true -> 
			pop_operators(Op, RestOp, [LastOp | ResultQueue]);
		false -> 
			{OpStack, ResultQueue}
	end.


push_operator(_, {[')' | _OpStackRest], _ResultQueue}) ->
	throw('error with parentheses');
push_operator(')', {['(' | OpStackRest], ResultQueue}) ->
	{OpStackRest, ResultQueue};
push_operator(Op, {OpStack, ResultQueue}) ->
	{[Op | OpStack], ResultQueue}.

