-module(re_lang).
-export([is_operator/1, is_concat/2, priority/1]).



token_class(T) when T == $?; T == $*; T == $+ -> 
						re_rep;
token_class($() -> 		re_left_par;
token_class($)) -> 		re_right_par;
token_class($|) -> 		re_alt;
token_class($.) -> 		re_con;
token_class(re_begin) ->re_begin;
token_class(re_end) -> 	re_end;
token_class(_) ->		re_letter. % not operator means letter


-spec is_operator(term()) -> boolean().

is_operator(re_begin) -> false;
is_operator(re_end) -> true;
is_operator(T) ->
	case (token_class(T)) of 
		re_letter -> false;
		_ -> true
	end.


-spec is_concat(term(), term()) -> boolean().

is_concat(PrevToken, Token) ->
	case {token_class(PrevToken), token_class(Token)} of 
		{re_right_par,	re_left_par} ->		true;
		{re_right_par,	re_letter} ->		true;
		{re_rep, 		re_left_par} ->		true;
		{re_rep,		re_letter} ->		true;
		{re_letter,		re_left_par} ->		true;
		{re_letter,		re_letter} ->		true;
		_ -> 								false
	end.


-spec priority(term()) -> integer().

priority(Op) ->
	case (token_class(Op)) of 
		re_right_par -> 0;
		re_end -> 		1;
		re_alt -> 		2;
		re_con -> 		3;
		re_left_par -> 	4;
		re_rep -> 		5
	end.
