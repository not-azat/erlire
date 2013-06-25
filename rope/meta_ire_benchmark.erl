-module(meta_ire_benchmark).
-export([run/0, permutations/0]).


files() ->
	[
		% doubling regex like a*a?a?a?aaaa*, constant str size:
		{"test_data/bench_1.txt", "test_data/bench_1_re.txt"},
		{"test_data/bench_2.txt", "test_data/bench_2_re.txt"},
		{"test_data/bench_3.txt", "test_data/bench_3_re.txt"},
		% one regex, str size doubles:
		{"test_data/bench_4.txt", "test_data/bench_4_re.txt"},
		{"test_data/bench_5.txt", "test_data/bench_5_re.txt"},
		{"test_data/bench_6.txt", "test_data/bench_6_re.txt"},
		{"test_data/bench_7.txt", "test_data/bench_7_re.txt"},
		{"test_data/bench_8.txt", "test_data/bench_8_re.txt"}
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
	io:format("serial for ~p~nstr size: ~p~nregex size: ~p~n", [FileName, size(Binary), size(AutomataStr)]),
	T1 = erlang:now(),
	_Result = re_processor:run(Binary, re_compiler:compile(AutomataStr)),
	T2 = erlang:now(),
	T = timer:now_diff(T2, T1),
	io:format("result: ~p~n", [T]).
	
re(FileName, Binary, AutomataStr) ->
	io:format("re for ~p~nstr size: ~p~nregex size: ~p~n", [FileName, size(Binary), size(AutomataStr)]),
	T1 = erlang:now(),
	{ok, MP} = re:compile(AutomataStr),
	_Result = re:run(Binary, MP),
	T2 = erlang:now(),
	T = timer:now_diff(T2, T1),
	io:format("result: ~p~n", [T]).
	
	
serial_ire(FileName, Binary, AutomataStr) ->
	io:format("serial_ire for ~p~nstr size: ~p~nregex size: ~p~n", [FileName, size(Binary), size(AutomataStr)]),
	T1 = erlang:now(),
	Automata = re_compiler:compile(AutomataStr),
	TransitionAutomata = re_transition:convert_automata(Automata),
	T2 = erlang:now(),
	Ire = ire:new(Binary, TransitionAutomata, 100),
	T3 = erlang:now(),
	_Result = ire:matches(Ire),
	T4 = erlang:now(),
	T = timer:now_diff(T4, T1),
	io:format("result: ~p~n", [timer:now_diff(T4, T1)]).


meta_ire(FileName, Binary, AutomataStr) ->
	io:format("meta_ire for ~p~nstr size: ~p~nregex size: ~p~n", [FileName, size(Binary), size(AutomataStr)]),
	T1 = erlang:now(),
	Automata = re_compiler:compile(AutomataStr),
	TransitionAutomata = re_transition:convert_automata(Automata),
	Len = erlang:max(byte_size(Binary) div 100, 300), 
	MetaIre = meta_ire:new(Binary, TransitionAutomata, Len),
	Result = meta_ire:matches(MetaIre),
	T2 = erlang:now(),
	T = timer:now_diff(T2, T1),
	io:format("result: ~p~n", [T]).


permutations() ->
	{ok, Bin} = file:read_file("test_data/perm.txt"),
	{ok, ReBin} = file:read_file("test_data/perm_re.txt"),
	Len = size(Bin),
	N = 7,
	K = Len div N,
	Bins = split_by(Bin, K, []),
	All1 = ire_perms(Bins, ReBin),
	All2 = meta_ire_perms(Bins, ReBin),
	All3 = re_perms(Bins, ReBin),
	All4 = seq_perms(Bins, ReBin).


re_perms(Bins, ReBin) ->
	io:format("perms re for ~p~n", [ReBin]),
	T1 = erlang:now(),
	{ok, MP} = re:compile(ReBin),
	AllPerms = perms(Bins),
	_Result = [ re:run(list_to_binary(P), MP) || P <- AllPerms ],
	T2 = erlang:now(),
	T = timer:now_diff(T2, T1),
	io:format("result: ~p~n", [T]).

seq_perms(Bins, ReBin) ->
	io:format("perms seq for ~p~n", [ReBin]),
	T1 = erlang:now(),
	Auto = re_compiler:compile(ReBin),
	AllPerms = perms(Bins),
	_Result = [ re_processor:run(list_to_binary(P), Auto) || P <- AllPerms ],
	T2 = erlang:now(),
	T = timer:now_diff(T2, T1),
	io:format("result: ~p~n", [T]).


ire_perms(Bins, ReBin) ->
	io:format("perms ire for ~p~n", [ReBin]),
	T1 = erlang:now(),
	Auto = re_compiler:compile(ReBin),
	Trans = re_transition:convert_automata(Auto),
	Ires = [ ire:new(B, Trans, 80) || B <- Bins ],
	T2 = erlang:now(),
	IresPerms = perms(Ires),
	_Result = [ merge_all(P) || P <- IresPerms ],
	T3 = erlang:now(),
	T = timer:now_diff(T2, T1),
	io:format("result: ~p ~p~n", [timer:now_diff(T2, T1), timer:now_diff(T3, T2)]).

merge_all([First | Ires]) ->
	Result = lists:foldl(
		fun(I, AccI) ->
			ire:merge(AccI, I)
		end,
		First,
		Ires),
	ire:matches(Result).

meta_ire_perms(Bins, ReBin) ->
	io:format("perms meta_ire for ~p~n", [ReBin]),
	T1 = erlang:now(),
	Auto = re_compiler:compile(ReBin),
	Trans = re_transition:convert_automata(Auto),
	Ires = [ meta_ire:new(B, Trans, 200) || B <- Bins ],
	IresPerms = perms(Ires),
	_Result = [ meta_merge_all(P) || P <- IresPerms ],
	T2 = erlang:now(),
	T = timer:now_diff(T2, T1),
	io:format("result: ~p~n", [T]).

meta_merge_all([First | Ires]) ->
	Result = lists:foldl(
		fun(I, AccI) ->
			meta_ire:merge(AccI, I)
		end,
		First,
		Ires),
	meta_ire:matches(Result).


split_by(Bin, K, Acc) when size(Bin) =< K -> 
	lists:reverse([Bin | Acc]);
split_by(Bin, K, Acc) ->
	<<B:K/binary, Rest/binary>> = Bin,
	split_by(Rest, K, [B | Acc]).


perms([]) -> [[]];
perms(L)  -> [[H|T] || H <- L, T <- perms(L -- [H])].


