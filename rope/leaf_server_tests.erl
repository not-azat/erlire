-module(leaf_server_tests).
-include_lib("eunit/include/eunit.hrl").
-import(leaf_server, [start/3, get_transition/1, dispose/1]).

transition_presents_test_() ->
	{setup,
		fun transition_presents_start/0,
		fun transition_presents_stop/1,
		fun transition_presents_inst/1}.

transition_presents_start() ->
	Automata = re_compiler:compile(<<"(abc+)*e|f">>),
	TransitionAutomata = re_transition:convert_automata(Automata),
	start(<<"gobigobiaccabede">>, TransitionAutomata, 5).

transition_presents_stop(Pid) ->
	dispose(Pid).

transition_presents_inst(Pid) ->
	Response = get_transition(Pid),
	?_assertMatch(_, Response).



transition_compose_test_() ->
	{setup,
		fun transition_compose_start/0,
		fun transition_compose_stop/1,
		fun transition_compose_inst/1}.

transition_compose_start() ->
	Automata = re_compiler:compile(<<"c*abcd*">>),
	TransitionAutomata = re_transition:convert_automata(Automata),
	Pid1 = start(<<"cccccccccab">>, TransitionAutomata, 5),
	Pid2 = start(<<"cddddd">>, TransitionAutomata, 5),
	{Pid1, Pid2, TransitionAutomata}.

transition_compose_stop({Pid1, Pid2, _}) ->
	dispose(Pid1),
	dispose(Pid2).

transition_compose_inst({Pid1, Pid2, TransitionAutomata}) ->
	Transition1 = get_transition(Pid1),
	Transition2 = get_transition(Pid2),
	Transition = re_transition:compose_transitions(Transition1, Transition2),
	io:format("Transition1:~n~p~nTransition2:~n~p~nTransition:~n~p~n", [Transition1, Transition2, Transition]),
	?_assert(re_transition:matches(Transition, TransitionAutomata)).



