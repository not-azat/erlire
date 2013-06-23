-module(meta_ire_benchmark).
-export([run/0]).


files() ->
	[
		{"test_data/bench_1.txt", "test_data/bench_1_re.txt"}
		%{"test_data/bench_2.txt", "test_data/bench_2_re.txt"},
		%{"test_data/bench_3.txt", "test_data/bench_3_re.txt"}
	].

run() ->
	lists:map(fun({FileName, ReFileName}) ->
			{ok, Bin} = file:read_file(FileName),
			{ok, ReBin} = file:read_file(ReFileName),
			serial(FileName, Bin, ReBin),
			re(FileName, Bin, ReBin),
			serial_ire(FileName, Bin, ReBin),
			meta_ire(FileName, Bin, ReBin)
		end,
		files()).

serial(FileName, Binary, AutomataStr) ->
	io:format("serial for ~p~n", [FileName]),
	T1 = erlang:now(),
	Result = re_processor:run(Binary, re_compiler:compile(AutomataStr)),
	T2 = erlang:now(),
	T = timer:now_diff(T2, T1),
	io:format("result: ~p~n", [T]),
	T.
	
re(FileName, Binary, AutomataStr) ->
	io:format("re for ~p~n", [FileName]),
	T1 = erlang:now(),
	{ok, MP} = re:compile(AutomataStr),
	Result = re:run(Binary, MP),
	T2 = erlang:now(),
	T = timer:now_diff(T2, T1),
	io:format("result: ~p~n", [T]),
	T.
	
	
serial_ire(FileName, Binary, AutomataStr) ->
	io:format("serial_ire for ~p~n", [FileName]),
	T1 = erlang:now(),
	Automata = re_compiler:compile(AutomataStr),
	TransitionAutomata = re_transition:convert_automata(Automata),
	Ire = ire:new(Binary, TransitionAutomata, 5),
	Result = ire:matches(Ire),
	T2 = erlang:now(),
	T = timer:now_diff(T2, T1),
	io:format("result: ~p~n", [T]),
	T.


meta_ire(FileName, Binary, AutomataStr) ->
	io:format("meta_ire for ~p~n", [FileName]),
	T1 = erlang:now(),
	Automata = re_compiler:compile(AutomataStr),
	TransitionAutomata = re_transition:convert_automata(Automata),
	Len = max(byte_size(Binary) div 100, 300), 
	MetaIre = meta_ire:new(Binary, TransitionAutomata, Len),
	Result = meta_ire:matches(MetaIre),
	T2 = erlang:now(),
	T = timer:now_diff(T2, T1),
	io:format("result: ~p~n", [T]),
	T.

max(A, B) when A > B -> A;
max(A, B) -> B.


