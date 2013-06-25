-module(leaf_server).
-export([start/3, start/2, get_transition/1, get_ire/1, dispose/1, run/0]).

-behaviour(gen_server).
-export([init/1, handle_cast/2, handle_call/3, terminate/2, handle_info/2, code_change/3]).


%%% Interface

start(BinString, TransAutomata, LeafLength) -> 
	{ok, Pid} = gen_server:start_link(?MODULE, {raw, BinString, TransAutomata, LeafLength}, []),
	gen_server:cast(Pid, build_ire),
	Pid.

start(Ire, TransAutomata) ->
	{ok, Pid} = gen_server:start_link(?MODULE, {ire, Ire, TransAutomata}, []),
	Pid.

get_transition(Pid) -> 
	gen_server:call(Pid, transition, 120000).

get_ire(Pid) ->
	gen_server:call(Pid, ire, 120000).

dispose(Pid) ->
	gen_server:cast(Pid, terminate).


%%% behaviour callbacks

init({raw, BinString, TransAutomata, LeafLength}) ->
	% io:format("init: ~n~p~n", [{raw, BinString, TransAutomata, LeafLength}]),
	{ok, {raw, BinString, TransAutomata, LeafLength}};
init({ire, Ire, TransAutomata}) ->
	% io:format("init: ~n~p~n", [{ire, Ire, TransAutomata}]),
	{ok, {ire, Ire, TransAutomata}}.

handle_cast(build_ire, {raw, BinString, TransAutomata, LeafLength}) ->
	% io:format("handle_cast: ~n~p~n", [{raw, BinString, TransAutomata, LeafLength}]),
	Ire = ire:new(BinString, TransAutomata, LeafLength),
	{noreply, {ire, Ire, TransAutomata}};
handle_cast(terminate, State) ->
	% io:format("handle_cast terminate: ~n~p~n", [State]),
	{stop, normal, State}.

handle_call(transition, _From, {ire, Ire, TransAutomata}) ->
	% io:format("handle_call: transition ~n~p~n", [{ire, Ire, TransAutomata}]),
	{reply, ire:get_transition(Ire), {ire, Ire, TransAutomata}};

handle_call(ire, _From, {ire, Ire, TransAutomata}) ->
	% io:format("handle_call: ire ~n~p~n", [{ire, Ire, TransAutomata}]),
	{reply, Ire, {ire, Ire, TransAutomata}}.

handle_info(Message, State) ->
	% io:format("handle_info: ~n~p~n", [State]),
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    %% No change planned.
    {ok, State}. 

terminate(_, State) ->
	% io:format("terminate: ~n~p~n", [State]),
	% % io:format("terminate. State: ~p~n", [State]),
	ok.



%%% debug

run() ->
	Automata = re_compiler:compile(<<"abcd">>),
	TransitionAutomata = re_transition:convert_automata(Automata),
	Pid1 = start(<<"ab">>, TransitionAutomata, 5),
	Pid2 = start(<<"cd">>, TransitionAutomata, 5),

	Transition1 = get_transition(Pid1),
	Transition2 = get_transition(Pid2),
	Transition = re_transition:compose_transitions(Transition1, Transition2),
	% io:format("Transition1:~n~p~nTransition2:~n~p~nTransition:~n~p~n", [Transition1, Transition2, Transition]),
	% io:format("Result: ~p~n", [re_transition:matches(Transition, TransitionAutomata)]),

	dispose(Pid1),
	dispose(Pid2).

