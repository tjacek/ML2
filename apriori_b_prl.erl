-module(apriori_b_prl).
%% balance producing candidates
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
-export([apriori_worker/2]).
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
	?LOG("create dicts\n", []),
	DataPortions = split_data(Data, DataWorkers),
	MetaData = [ get_meta(DataPortion, 1, dict:new()) || DataPortion <- DataPortions ],
	
	?LOG("before spawn\n", []),	
	WorkerIds = [ {{DataWorker, ItemsetWorker}, supervisor_manager:compute(apriori_worker, [lists:nth(DataWorker, MetaData), DataWorker])} 
			|| DataWorker <- lists:seq(1,DataWorkers),
			   ItemsetWorker <- lists:seq(1,ItemsetWorkers)],
	MetaCounts = [dict:map(fun (_Key, Val) -> length(Val) end, Meta) || Meta <- MetaData ],
	
	apriori_first(glue_meta(MetaCounts), WorkerIds, DataWorkers, ItemsetWorkers, MinSup, MinConf, dict:new()).

glue_meta([Dict | Dicts]) ->
	glue_meta(Dicts, Dict).

glue_meta([] = _Dicts, Result) ->
	Result;
glue_meta([Dict | Dicts], Result) ->
	glue_meta(Dicts, dict:merge(fun (_K, V1, V2) -> V1 + V2 end, Result, Dict)).

get_meta(_A = [], _I, Res) ->
	Res;
get_meta(_A = [Head | Tail], I, Res) ->
	get_meta(Tail, I+1, get_meta0(Head, I, Res)).

get_meta0(_A = [], _I, Dict) ->
	Dict;
get_meta0(_A = [Head | Tail], I, Dict) ->
	get_meta0(Tail, I, dict:append([Head], I, Dict)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% @doc Worker thread for finding associative rules
apriori_worker(MetaData, DataId) ->
	io:format("I'm alive ~p\n", [self()]),
%	io:format("My Meta ~p\n", [MetaData]),

	receive 
		{_, stop} ->
			io:format("Stop ~p\n", [self()]),
			ok;
		{Pid, {Dict, Id}} ->
			CDict = count(dict:fetch_keys(Dict), MetaData, []),
%			io:format("~p result: ~p\n", [self(), dict:to_list(CDict)]),
			Pid ! {Id, dict:map(fun (_Key, Val) -> {[DataId], Val} end, CDict)},
			apriori_worker(MetaData, DataId)
	end.

count([] = _ListOfSets, _Dict, Acu) ->
	dict:from_list([{Key, length(Transactions)} || {Key, Transactions} <- Acu]);
count([H|T] = _ListOfSets, Dict, Acu) ->
	count(T, Dict, [{H, count_intersect(H, Dict)}|Acu]).


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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% @doc Result : {Antecedent,Consequent,support(Antecedent),support(Antecedent u Consequent),Confidence}
apriori_first(DictData, Workers, DataWorkers, ItemsetWorkers, MinSup, MinConf, Result) ->
	PrunedLk = prune(DictData, MinSup),
	?LOG("PrunedLk ~p\n", [dict:to_list(PrunedLk)]),
	
	% make new
	Initial = dict:fetch_keys(PrunedLk),

	NewSets = make_sets(PrunedLk, 2, ItemsetWorkers),
	?LOG("NewSets~p\n", [NewSets]),
	apriori_steps(2, Initial, NewSets, Workers, DataWorkers, ItemsetWorkers, MinSup, MinConf, dict:merge(fun (K, V1, V2) -> ?LOG("dicte merge error ~p, ~p, ~p \n",[K, V1, V2]) end, PrunedLk, Result)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
apriori_steps(_Step, _Initial, [], Workers, _DataWorkers, _ItemsetWorkers, _MinSup, MinConf, ResultDict) ->
	stop_workers(Workers),
	?LOG("ResultDict ~p\n", [dict:to_list(ResultDict)]),
	Itemsets = dict:to_list(ResultDict),
	?LOG("Generate Rules \n", []),
	Rules = generate_rules(Itemsets, []),
	?LOG("Prune Rules \n", []),
	prune_rules(Rules, ResultDict, MinConf, []);
apriori_steps(Step, Initial, Data, Workers, DataWorkers, ItemsetWorkers, MinSup, MinConf, Result) ->
	Dicts = get_dicts(Data),
	% count and prune
	DictLk = let_workers_count(Workers, Dicts),
	?LOG("To prune ~p\n", [dict:to_list(DictLk)]),
	PrunedLk = prune(DictLk, MinSup),
	?LOG("PrunedLk ~p\n", [dict:to_list(PrunedLk)]),
	
	% make new
	NewSets = make_sets(PrunedLk, Step+1, ItemsetWorkers),
	?LOG("NewSets~p\n", [NewSets]),
	apriori_steps(Step+1,Initial, NewSets, Workers, DataWorkers, ItemsetWorkers, MinSup, MinConf, dict:merge(fun (K, V1, V2) -> ?LOG("dict merge error ~p, ~p, ~p \n",[K, V1, V2]) end, PrunedLk, Result)).
	% if empty -> finish

get_dicts(Data) ->
	% Professor Arne Andersson's General Balanced Trees, works well with a lot of data
	[initial_counters(dict:new(), gb_sets:to_list(gb_sets:from_list(DataSet))) || DataSet <- Data ].
	% returns updated Dict


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
let_workers_count(Workers, Dicts) ->
	send_dict(Workers, Dicts),
	?LOG("Receive\n", []),
	receive_and_merge(Workers, dict:new()).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
send_dict([], _Dicts) ->
	ok;
send_dict([{{_DataId, CandidateId}, Worker}|Workers], Dicts) ->
	case supervisor_manager:message(Worker, {lists:nth(CandidateId, Dicts), Worker}) of
		ok -> send_dict(Workers, Dicts);
		{error, Reason} -> {error, Reason}
	%Worker ! {self(), Dict},
	end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
receive_and_merge([], Result) ->
	dict:map(fun (_Key, {_DataIds, Count}) -> Count end, Result);
receive_and_merge([{_, Worker}|Workers], Result) ->
	receive {Worker, Dict} ->
		NewResult = dict:merge(
				fun (_K, {[DataId], V1}, {DataIds, V2}) -> 
					case is_subset([DataId], DataIds) of
						true -> {DataIds, V2};
						false -> {lists:usort([DataId|DataIds]), V1 + V2} 
					end
				end,   
				Result,
				Dict),
		receive_and_merge(Workers, NewResult)
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
stop_workers([]) ->
	ok;
stop_workers([{_, Worker}|Workers]) ->
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
prune(Dict, MinSup) ->
	dict:filter(fun(_ItemSet, Count) -> Count >= MinSup end, Dict).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
make_sets(Dict, Size, ItemsetWorkers) ->
	DickList = lists:sort(fun ({S1, V1}, {S2, V2}) -> V1 =< V2 end, dict:to_list(Dict)),
	case DickList of
		[] -> [];
		_ ->
			Sum = lists:foldl(fun ({S1, V1}, Acc) -> Acc + V1 end, 0, DickList),
			ItemSets = [ {[], 0} || _Enumerate <- lists:seq(1,ItemsetWorkers) ],

			balance(DickList, Sum, Size, ItemSets)
	end.
%%%
balance([{Set, V}], Sum, Size, ItemsetWorkers) ->
	lists:map(fun ({S, V}) -> gb_sets:to_list(gb_sets:from_list(S)) end, ItemsetWorkers);
balance([{Set, V}|DickList], Sum, Size, ItemsetWorkers) ->
	Estimate = (Sum - V) * V,
	ToReplace = get_with_lowest_todo(ItemsetWorkers),
	balance(DickList, Sum-V, Size, add_estimate(ToReplace, Estimate, Set, DickList, Size, ItemsetWorkers)).

get_with_lowest_todo([{Set, Val}|ItemsetWokers]) ->
	get_with_lowest_todo(ItemsetWokers, 2, {1, Val}).

get_with_lowest_todo([] = _ItemsetWokers, _I, {N, Min}) ->
	N;
get_with_lowest_todo([{_Set, Val}|ItemsetWokers], I, {_N, Min}) when Val < Min->
	get_with_lowest_todo(ItemsetWokers, I+1, {I, Val});
get_with_lowest_todo([{_Set, Val}|ItemsetWokers], I, {N, Min}) when Val >= Min->
	get_with_lowest_todo(ItemsetWokers, I+1, {N, Min}).

add_estimate(ToReplace, Estimate, Set, DickList, Size, ItemsetWorkers) ->
	add_estimate(ToReplace, Estimate, 1, Set, lists:map(fun ({S, V}) -> S end, DickList), Size, ItemsetWorkers).

add_estimate(ToReplace, Estimate, ToReplace, Set, DickList, Size, [{S, V}|ItemsetWorkers]) ->
	[{combinations([Set], DickList, Size, []) ++ S, V+Estimate}|ItemsetWorkers];
add_estimate(ToReplace, Estimate, I, Set, DickList, Size, [Head|ItemsetWorkers]) when I < ToReplace->
	[Head|add_estimate(ToReplace, Estimate, I+1, Set, DickList, Size, ItemsetWorkers)].

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
	io:format("Margins ~p\n", [Margins]),
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
	lists:sort(fun({_,_,_,_,ConfA}, {_,_,_,_,ConfB}) -> ConfA >= ConfB end, Acc) ;
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

