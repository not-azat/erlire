-module(re_lang).
-export([is_operator/1, is_concat/2, priority/1]).



token_class(T) when T == '?'; T == '*'; T == '+' -> 
							rep;
token_class('(') -> 		'(';
token_class(')') -> 		')';
token_class('|') -> 		'|';
token_class('concat') -> 	'concat';
token_class('begin') ->		'begin';
token_class('end') -> 		'end';
token_class(_) ->			letter. % not operator means letter


-spec is_operator(term()) -> boolean().

is_operator('begin') -> false;
is_operator('end') -> true;
is_operator(T) ->
	case (token_class(T)) of 
		letter -> false;
		_ -> true
	end.


-spec is_concat(term(), term()) -> boolean().

is_concat(PrevToken, Token) ->
	case {token_class(PrevToken), token_class(Token)} of 
		{')',	'('} ->		true;
		{')',	letter} ->	true;
		{rep, 	'('} ->		true;
		{rep,	letter} ->	true;
		{letter,'('} ->		true;
		{letter,letter} ->	true;
		_ -> 				false
	end.


-spec priority(term()) -> integer().

priority(Op) ->
	case (token_class(Op)) of 
		')' ->		0;
		'end' -> 	1;
		'|' -> 		2;
		concat -> 	3;
		'(' ->		4;
		rep ->		5
	end.
