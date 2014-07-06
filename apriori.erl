-module(apriori).
%% itemsets sent over network
%%
%% Include files
%%

-include("structures.hrl").
-include("debug.hrl").

%%
%% Exported Functions
%%

-export([mine/2, test/4, testseq/3]).
%% paralel
-export([apriori_worker/0, meta_worker/2]).
%%
%% API Functions
%%

-spec mine(Data :: [ [any()] ], Options :: []) -> [{Antecedent :: [any()],Consequent :: [any()],SupAntecedent::number(),SupNominator::number(),Confidence::number()}] | [{FrequentSubsequence :: [any()], Support :: number()}].
% @spec mine(Data, Options) -> [{Antecedent,Consequent,SupAntecedent,SupNominator,Confidence}] | [{FrequentSubsequence, Support}] where Data = [ [any()] ], Options = [], Antecedent = [any()],Consequent = [any()],SupAntecedent = number(),SupNominator=number(),Confidence=number(), FrequentSubsequence = [any()], Support = number()
% @doc Performs the data mining in the data. Generates associative rules describing the data or frequent sequences. Possible options are: * {min_sup, MinSup} - MinimalSupport, * {workers, Workers} - number of working threads, * {min_conf, MinConf} - MinimalConfidence, * seq - search for frequent sequences
mine(Data, Options) ->
	MinSup = case lists:keyfind(min_sup, 1, Options) of
		false -> 0; % default
		{min_sup, MinSupVal} -> MinSupVal * length(Data)
		end,
	DatasetWorkers = case lists:keyfind(dataset_workers, 1, Options) of
		false -> 1; % default
		{dataset_workers, WorkersNo} -> WorkersNo
		end,
	ItemsetWorkers = case lists:keyfind(itemset_workers, 1, Options) of
		false -> 1; % default
		{itemset_workers, WorkersNo1} -> WorkersNo1
		end,
	MinConf = case lists:keyfind(min_conf, 1, Options) of
		false -> 0; % default
		{min_conf, MinConfVal} -> MinConfVal
		end,

%	Dict = get_dict(Data),
	freq_apriori(Data, DatasetWorkers, ItemsetWorkers, MinSup, MinConf).

freq_apriori(Data, DataWorkers, ItemsetWorkers, MinSup, MinConf) ->
	DataPortions = split_data(Data, DataWorkers),
	MetaData = build_meta(DataPortions, 1, []),

	?LOG("before spawn\n", []),	
	WorkerIds = [ supervisor_manager:compute(apriori_worker, []) 
			|| DataWorker <- lists:seq(1,DataWorkers),
			   ItemsetWorker <- lists:seq(1,ItemsetWorkers)],
	?INFO("~p workers spawned\n", [length(WorkerIds)]),
	
	apriori_first(MetaData, WorkerIds, DataWorkers, ItemsetWorkers, MinSup, MinConf, dict:new()).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% @doc Result : {Antecedent,Consequent,support(Antecedent),support(Antecedent u Consequent),Confidence}
apriori_first(DictData, Workers, DataWorkers, ItemsetWorkers, MinSup, MinConf, Result) ->
	{Results, PrunedLk} = prune(DictData, MinSup),
	?LOG("Results ~p\nPrunedLk ~p\n", [Results, PrunedLk]),
	
	case lists:flatten(PrunedLk) of
		[] -> apriori_steps(2, [], Workers, DataWorkers, ItemsetWorkers, MinSup, MinConf, Results);
		_ -> apriori_steps(2, PrunedLk, Workers, DataWorkers, ItemsetWorkers, MinSup, MinConf, Results)
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
apriori_steps(_Step, [], Workers, _DataWorkers, _ItemsetWorkers, _MinSup, MinConf, ResultDict) ->
	stop_workers(Workers),
	?LOG("ResultDict ~p\n", [dict:to_list(ResultDict)]),
	Itemsets = lists:sort(fun ({KA, SuppA}, {KB,SuppB}) -> (SuppA > SuppB) or ((SuppA == SuppB) and (length(KA) > length(KB))) end, dict:to_list(ResultDict));
	%?LOG("Generate Rules \n", []),
	%Rules = generate_rules(Itemsets, []),
	%?LOG("Prune Rules \n", []),
	%prune_rules(Rules, ResultDict, MinConf, []);
apriori_steps(Step, PrunedList, Workers, DataWorkers, ItemsetWorkers, MinSup, MinConf, Result) ->
	?INFO("Step ~p\n", [Step]),
%	io:format("PrunedList : ~p\n ~p\n", [length(PrunedList), PrunedList]),
	Sets = make_sets(PrunedList, ItemsetWorkers),
%	io:format("Sets : ~p\n ~p\n", [length(Sets), Sets]),
	% count and prune
	DictLk = let_workers_count(Workers, Sets),
	?LOG("To prune ~p\n", [DictLk]),
	{NewResult, PrunedLk} = prune(DictLk, MinSup),
	?LOG("PrunedLk ~p\n", [PrunedLk]),
%	io:format("Result : ~p\n", [Result]),
%	io:format("NewResult : ~p\n", [NewResult]),
	case lists:flatten(PrunedLk) of
		[] -> 	apriori_steps(Step+1, [], Workers, DataWorkers, ItemsetWorkers, MinSup, MinConf, dict:merge(fun (K, V1, V2) -> ?LOG("dict merge error ~p, ~p, ~p \n",[K, V1, V2]) end, NewResult, Result));
		_ -> apriori_steps(Step+1, PrunedLk, Workers, DataWorkers, ItemsetWorkers, MinSup, MinConf, dict:merge(fun (K, V1, V2) -> ?LOG("dict merge error ~p, ~p, ~p \n",[K, V1, V2]) end, NewResult, Result))
	end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% @doc Worker thread for finding associative rules
apriori_worker() ->
	?INFO("I'm alive ~p\n", [self()]),

	receive 
		{_, stop} ->
			?INFO("Stop ~p\n", [self()]),
			ok;
		{Pid, {{SetsToCheck, MetaData}, DataId, Id}} ->
			?INFO("~p Got dataId ~p\n", [self(), DataId]),
			MetaDict = dict:from_list(MetaData),
			CDict = count(SetsToCheck, MetaDict),
			Pid ! {Id, DataId, dict:map(fun (_Key, Val) -> {[DataId], Val} end, CDict)},
			apriori_worker()
	end.

% returns DataWorkersNo dicts Set -> {[DataId], Transactions}
build_meta([] = _DataPortions, _I, Acu) ->
	receive_meta(Acu, []);
build_meta([Portion|DataPortions], I, Acu) ->
	build_meta(DataPortions, I + length(Portion), [supervisor_manager:compute(meta_worker, [I, Portion])|Acu]).

receive_meta([], Acu) ->
	[ dict:map( fun (_K, V) -> {[I], V} end, Dict )
	|| {I, Dict} <- lists:zip(lists:seq(1,length(Acu)), Acu) ];
receive_meta([WorkerId|Workers], Acu) ->
	receive 
		{result, WorkerId, Result} ->
			receive_meta(Workers, [Result|Acu])
	end.
			

meta_worker(I, Portion) ->
	get_meta(Portion, I, dict:new()).

glue_meta(Dicts) ->
	glue_meta(Dicts, dict:new()).

glue_meta([] = _Dicts, Result) ->
	Result;
glue_meta([Dict | Dicts], Result) ->
	glue_meta(Dicts, dict:merge(
				fun (_K, {DataIds, V1}, {[DataId], V2}) -> 
					case is_subset([DataId], DataIds) of
						true -> {DataIds, V1};
						false -> {lists:usort([DataId|DataIds]), V1 ++ V2}
					end
				end, 
			        Result, 
				Dict)).

get_meta(_A = [], _I, Res) ->
	Res;
get_meta(_A = [Head | Tail], I, Res) ->
	get_meta(Tail, I+1, get_meta0(Head, I, Res)).

get_meta0(_A = [], _I, Dict) ->
	Dict;
get_meta0(_A = [Head | Tail], I, Dict) ->
	get_meta0(Tail, I, dict:append([Head], I, Dict)).

			
count([] = _ListOfSets, _MetaDict) ->
	dict:new();
count([{Lead, _With}|_T] = ListOfSets, MetaDict) ->
	count(ListOfSets, MetaDict, length(Lead) + 1, dict:new()).

count([] = _ListOfSets, MetaDict, Size, Result) ->
	Result;
count([{Lead, With}|T] = _ListOfSets, MetaDict, Size, Result) ->
	count(T, 
              MetaDict, 
              Size, 
              count_this({Lead, With}, MetaDict, Size, Result)).

count_this({Lead, []}, MetaDict, Size, Result) ->
	Result;
count_this({Lead, [With|T]}, MetaDict, Size, Result) ->
	Check = lists:usort(Lead++With),
	case (length(Check) == Size) and not dict:is_key(Check, Result) of
		true -> count_this({Lead, T}, 
				   MetaDict, 
				   Size, 
				   dict:store(Check, 
					      intersect(dict:fetch(Lead, MetaDict), 
				                        dict:fetch(With, MetaDict)), 
                                              Result));
		false -> count_this({Lead, T}, MetaDict, Size, Result)
	end.

count_intersect([] = _ListToCheck, _Dict) ->
	[];
count_intersect([H1] = _ListToCheck, Dict) ->
	case dict:is_key([H1], Dict) of
		true -> dict:fetch([H1], Dict);
		false -> []
	end;
count_intersect([H1,H2|T] = _ListToCheck, Dict) ->
	case dict:is_key([H1], Dict) and dict:is_key([H2], Dict) of
		true -> count_intersect0(T, Dict, intersect(dict:fetch([H1], Dict),dict:fetch([H2], Dict)));
		false -> []
	end.
	
count_intersect0([] = _ListToCheck, _Dict, Res) ->
	Res;
count_intersect0([H|T] = _ListToCheck, Dict, Res) ->
	case dict:is_key([H], Dict) of
		true -> count_intersect0(T, Dict, intersect(Res, dict:fetch([H], Dict)));
		false -> []
	end.

intersect(A, B) ->
	intersect0(A,B,[]).

intersect0([] = _A, _B, Res) ->
	lists:reverse(Res);
intersect0(_A, [] = _B, Res) ->
	lists:reverse(Res);
intersect0([A1|At] = _A, [A1|Bt] = _B, Res) ->
	intersect0(At, Bt, [A1|Res]);
intersect0([A1|At] = _A, [B1|_Bt] = B, Res) when A1 < B1->
	intersect0(At, B, Res);
intersect0([A1|_At] = A, [B1|Bt] = _B, Res) when A1 > B1->
	intersect0(A, Bt, Res).

get_dicts(Data) ->
	% Professor Arne Andersson's General Balanced Trees, works well with a lot of data
	[initial_counters(dict:new(), gb_sets:to_list(gb_sets:from_list(DataSet))) || DataSet <- Data ].
	% returns updated Dict


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
let_workers_count(Workers, PrunedList) ->
	send_dict(Workers, PrunedList),
	?LOG("Receive\n", []),
	receive_and_merge(Workers, dict:new()).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
send_dict(Workers, Data) ->
	send_ith_dict(Workers, Data, 1).

send_ith_dict([], [], DataId) ->
	ok;
send_ith_dict([Worker|Workers], [], DataId) ->
	case supervisor_manager:message(Worker, {{[], []}, DataId, Worker}) of
		ok -> send_ith_dict(Workers,[],DataId+1);
		{error, Reason} -> io:format("EROR\n"), {error, Reason}
	end;
send_ith_dict(Workers, [DataPortion|Data], DataId) ->
	send_ith_dict(send_ith_portion(Workers, DataPortion, DataId),
	              Data,
		      DataId+1).

send_ith_portion(Workers, [], _DataId) ->
	Workers;
send_ith_portion([Worker|Workers], [DataCandidates|Data], DataId) ->
	case supervisor_manager:message(Worker, {DataCandidates, DataId, Worker}) of
		ok -> send_ith_portion(Workers, Data, DataId);
		{error, Reason} -> io:format("EROR\n"), {error, Reason}
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
receive_and_merge([], Result) ->
	[ V || {_K, V} <- dict:to_list(Result)];
receive_and_merge([Worker|Workers], Result) ->
	receive {Worker, DataId, Dict} ->
		receive_and_merge(Workers, dict:update(DataId, fun (OldDict) -> glue_meta([Dict],OldDict) end, Dict, Result))
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
stop_workers([]) ->
	ok;
stop_workers([Worker|Workers]) ->
	%Worker ! stop,
	case supervisor_manager:message(Worker, stop) of
		ok -> stop_workers(Workers);
		{error, Reason} -> {error, Reason}
	end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
initial_counters(Dict, []) ->
	Dict;
initial_counters(Dict, [Elem|Data]) ->
	Dict1 = dict:store(Elem, 0, Dict),
	initial_counters(Dict1, Data).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
is_subset([],_Set) ->
	true;
is_subset(_Subset, []) ->
	false;
is_subset([Subelem|_Subset], [Elem|_Set]) when Elem > Subelem ->
	false;
is_subset([Subelem|Subset], [Elem|Set]) when Elem < Subelem ->
	is_subset([Subelem|Subset], Set);
is_subset([Subelem|Subset], [Subelem|Set]) ->
	is_subset(Subset, Set).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% length(Dicts) == DataWorkers
prune(Dicts, MinSup) ->
%	io:format("In prune : Dicts ~p, flat ~p\n", [length(Dicts), Dicts]),
	Mapped = dict:map(fun(_ItemSet, {DataIds, Transactions}) -> length(Transactions) end, glue_meta(Dicts)),
	Filtered = dict:filter(fun(_ItemSet, L) -> L >= MinSup end, Mapped),
	{ Filtered,
		[ lists:filter(fun({ItemSet, _Transactions}) -> dict:is_key(ItemSet, Filtered) end, dict:to_list(Dict))
		|| Dict <- Dicts]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% length(PrunedDicts) == DataWorkers, returns [DataWorkers = [ItemsetWorkers]]
make_sets(PrunedDicts, ItemsetWorkers) ->
	make_sets(PrunedDicts, ItemsetWorkers, []).

make_sets([] = _PrunedDicts, _ItemsetWorkers, Acu) ->
	Acu;
make_sets([Dict|PrunedDicts], ItemsetWorkers, Acu) ->
	ForData = make_sets_per_worker(Dict, ItemsetWorkers),
	make_sets(PrunedDicts, ItemsetWorkers, [ForData|Acu]).

make_sets_per_worker(PrunedList, ItemsetWorkers) ->
	CountList = [ {Set, Items, length(Items)} || {Set, {_DataIds, Items}} <- PrunedList ],
	DickList = lists:sort(fun ({S1, V1, L1}, {S2, V2, L2}) -> L1 >= L2 end, CountList),
	case DickList of
		[] -> [];
		_ ->
			Sum = lists:foldl(fun ({S1, V1, L1}, Acc) -> Acc + L1 end, 0, DickList),
			ItemSets = [ {[], [], 0} || _Enumerate <- lists:seq(1,ItemsetWorkers) ],
			balance(DickList, Sum, ItemSets)
	end.
%%%
balance([{Set, V, L}], Sum, ItemsetWorkers) ->
	case ItemsetWorkers of
		[{[], [], 0} | T] -> lists:map(fun ({S, V, L}) -> {gb_sets:to_list(gb_sets:from_list(S)), V} end, add_estimate(1, 1, Set, [], ItemsetWorkers));
		_ -> lists:map(fun ({S, V, L}) -> {gb_sets:to_list(gb_sets:from_list(S)), V} end, ItemsetWorkers)
	end;
balance([{Set, V, L}|DickList] = Whole, Sum, ItemsetWorkers) ->
	Estimate = (Sum - L) * L,
	{ToReplace, CurrentEstimate} = get_with_lowest_todo(ItemsetWorkers),
	if 
		CurrentEstimate == 0 ->	balance(DickList, Sum-L, add_estimate(ToReplace, Estimate, {Set, Whole}, DickList, ItemsetWorkers));
		true -> balance(DickList, Sum-L, add_estimate(ToReplace, Estimate, Set, DickList, ItemsetWorkers))
	end.

get_with_lowest_todo([{Set, _Items, Val}|ItemsetWokers]) ->
	get_with_lowest_todo(ItemsetWokers, 2, {1, Val}).

get_with_lowest_todo([] = _ItemsetWokers, _I, {N, Min}) ->
	{N, Min};
get_with_lowest_todo([{_Set, _Items, Val}|ItemsetWokers], I, {_N, Min}) when Val < Min->
	get_with_lowest_todo(ItemsetWokers, I+1, {I, Val});
get_with_lowest_todo([{_Set, _Items, Val}|ItemsetWokers], I, {N, Min}) when Val >= Min->
	get_with_lowest_todo(ItemsetWokers, I+1, {N, Min}).

add_estimate(ToReplace, Estimate, {Set, Items}, DickList, ItemsetWorkers) ->
	add_estimate(ToReplace, Estimate, 1, {Set, lists:map(fun ({S, V, L}) -> {S, V} end, Items)}, lists:map(fun ({S, V, L}) -> S end, DickList), ItemsetWorkers);
add_estimate(ToReplace, Estimate, Set, DickList, ItemsetWorkers) ->
	add_estimate(ToReplace, Estimate, 1, Set, lists:map(fun ({S, V, L}) -> S end, DickList), ItemsetWorkers).

add_estimate(ToReplace, Estimate, ToReplace, {Set, Items}, DickList, [{S, [], V}|ItemsetWorkers]) ->
	[ {[{Set, DickList}|S], Items, V+Estimate}|ItemsetWorkers];
add_estimate(ToReplace, Estimate, ToReplace, Set, DickList, [{S, Items, V}|ItemsetWorkers]) ->
	[ {[{Set, DickList}|S], Items, V+Estimate}|ItemsetWorkers];
add_estimate(ToReplace, Estimate, I, Set, DickList, [Head|ItemsetWorkers]) when I < ToReplace->
	[Head|add_estimate(ToReplace, Estimate, I+1, Set, DickList, ItemsetWorkers)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
combinations([], _Tails, _Size, Acu) ->
	lists:usort(Acu);
combinations([Elem|Elems], Tails, Size, Acu) ->
	Products = [ lists:usort(Elem++E) || E <- Tails ],
	Valid = lists:filter(fun(L) -> length(L) == Size end, Products),
	combinations(Elems, Tails, Size, Valid++Acu).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
repeat_combinations([_Elem], _Initial, Acu) ->
	Acu;
repeat_combinations([Elem|Elems], Tails, Acu) ->
	Products = [ Elem++E || E <- Tails],
	repeat_combinations(Elems, Tails, Products++Acu).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
split_data(Data, WorkersNo) ->
	Lists = [[]],
	DataLen = length(Data),
	Margins = [ DataLen * Mul / WorkersNo || Mul <- lists:seq(1,WorkersNo)],
	?INFO("Margins ~p\n", [Margins]),
	split_data_acu(Data, Margins, 1, Lists).
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
split_data_acu([] = _Data, [_Margin], _I, Results) ->
	lists:reverse( [lists:reverse(Res) || Res <- Results] );
split_data_acu([Trans|DataTail], [Margin|_MarginsTail]=Margins, I, [CurrentRes|Others]) when I =< Margin->
	split_data_acu(DataTail, Margins, I+1, [[Trans|CurrentRes] | Others]);
split_data_acu([Trans|DataTail], [Margin|MarginsTail], I, Results) when I > Margin->
	split_data_acu(DataTail, MarginsTail, I+1, [[Trans] | Results]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
append_to_dict(Key, Value, Dict) ->
	dict:update(Key, fun (Old) -> [Value|Old] end, [Value], Dict).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
generate_rules([], Acc) ->
	Acc;
generate_rules([{Set, _Support}|Sets], Acc) ->
	generate_rules(Sets, subset(Set, [], [], Set, []) ++ Acc).
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
subset([], [] = _Left, _Current, _FullSet, Acc) ->
	Acc;
subset([], _Left, [] = _Current, _FullSet, Acc) ->
	Acc;
subset([], Left, Current, FullSet, Acc) ->
	[{lists:reverse(Left), lists:reverse(Current), FullSet}|Acc];
subset([Item|Set], Left, Current, FullSet, Acc) ->
	% occlusion of item
	Acc1 = subset(Set, [Item|Left], Current, FullSet, Acc),
	% join to current
	subset(Set, Left, [Item|Current], FullSet, Acc1).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
prune_rules([] = _Rules, _Dict, _MinConfidence, Acc) ->
	lists:sort(fun({_,_,_,MaxSupA,ConfA}, {_,_,_,MaxSupB,ConfB}) -> (ConfA > ConfB) or ((ConfA == ConfB) and (MaxSupA > MaxSupB)) end, Acc) ;
prune_rules([{Antecedent, Consequent, Nominator}|Rules], Dict, MinConfidence, Acc) ->
	SupAntecedent = dict:fetch(Antecedent, Dict),
	SupNominator = dict:fetch(Nominator, Dict),
	Confidence = SupNominator/SupAntecedent,
	if Confidence >= MinConfidence -> 
		prune_rules(Rules, Dict, MinConfidence, [{Antecedent,Consequent,SupAntecedent,SupNominator,Confidence}|Acc]);
	true -> prune_rules(Rules, Dict, MinConfidence, Acc)
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
test(MinSup, MinConf, Workers, Nodes) ->
	%Data = mllib:read_mine_data("mine_data"),
	{ok, Data} = mllib:read_mine_data("amazon_access.csv"),
	Options = [{min_sup, MinSup}, {workers, Workers}, {min_conf, MinConf}, Nodes],
	Result = mllib:mine(Data, apriori, Options, Nodes),
	?LOG("Result: ~w\n", [Result]),
	Result.


testseq(MinSup, Workers, Nodes) ->
	%Data = mllib:read_mine_data("mine_data"),
	{ok, Data} = mllib:read_mine_data("rest.txt"),
	Options = [{min_sup, MinSup}, {workers, Workers}, seq],
	Result = mllib:mine(Data, apriori, Options, Nodes),
	?LOG("Result: ~w\n", [Result]),
	Result.

