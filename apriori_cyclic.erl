-module(apriori_cyclic).

%%
%% Include files
%%

-include("structures.hrl").
-include("debug.hrl").

%%
%% Exported Functions
%%

-export([mine/2,extCall/1,test/4, testseq/3]).
%% paralel
-export([apriori_worker/1,experiment/5]).
%%
%% API Functions
%%

extCall(Args) ->
  MinSup= list_to_float(lists:nth(1,Args)), 
  MinConf=list_to_float(lists:nth(2,Args)),
  DatasetWorkers=list_to_integer(lists:nth(3,Args)), 
  ItemsetWorkers=list_to_integer(lists:nth(4,Args)), 
  DatasetName=lists:nth(5,Args),
  apriori_cyclic:experiment(MinSup, MinConf, DatasetWorkers,ItemsetWorkers,DatasetName). 

experiment(MinSup, MinConf,DatasetWorkers,ItemsetWorkers,DatasetName) ->
  {ok, Data} = mllib:read_mine_data(DatasetName),
  %Nodes= [node()],
  Options = [{min_sup, MinSup}, {dataset_workers, DatasetWorkers},     {itemset_workers,ItemsetWorkers}, {min_conf, MinConf}],
  %Result=apriori:mine(Data, Options),
  %Result = mllib:mine(Data, apriori, Options, Nodes),
  mllib:mine(Data, apriori_cyclic, Options).
  %Result = apriori_cyclic:mine(Data,Options).

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
	io:format("~p \n",[MetaData]),
	?LOG("before spawn\n", []),	
	WorkerIds = [ {{DataWorker, ItemsetWorker}, supervisor_manager:compute(apriori_worker, [lists:nth(DataWorker, MetaData)])} 
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
apriori_worker(MetaData) ->
	io:format("I'm alive ~p\n", [self()]),

	receive 
		{_, stop} ->
			io:format("Stop ~p\n", [self()]),
			ok;
		{Pid, {Dict, Id}} ->
			CDict = count(dict:fetch_keys(Dict), MetaData, []),
			Pid ! {Id, CDict},
			apriori_worker(MetaData)
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
		tfalse -> []
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

	NewSets = make_sets(PrunedLk, Initial, 2),
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
	Dicts = get_dicts(Data, ItemsetWorkers),
	% count and prune
	DictLk = let_workers_count(Workers, Dicts),
	%?LOG("To prune ~p\n", [dict:to_list(DictLk)]),
	PrunedLk = prune(DictLk, MinSup),
	?LOG("PrunedLk ~p\n", [dict:to_list(PrunedLk)]),
	
	% make new
	NewSets = make_sets(PrunedLk, Initial, Step+1),
	?LOG("NewSets~p\n", [NewSets]),
	apriori_steps(Step+1,Initial, NewSets, Workers, DataWorkers, ItemsetWorkers, MinSup, MinConf, dict:merge(fun (K, V1, V2) -> ?LOG("dict merge error ~p, ~p, ~p \n",[K, V1, V2]) end, PrunedLk, Result)).
	% if empty -> finish

get_dicts(Data, WorkersNo) ->
	UniqueData = gb_sets:to_list(gb_sets:from_list(Data)), % Professor Arne Andersson's General Balanced Trees, works well with a lot of data
	?DETAIL("UniqueData: ~p\n", [UniqueData]),
	Sets = split_data(UniqueData, WorkersNo),
	[initial_counters(dict:new(), DataSet) || DataSet <- Sets ].
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
	Result;
receive_and_merge([{_, Worker}|Workers], Result) ->
	receive {Worker, Dict} ->
		NewResult = dict:merge(fun (_K, V1, V2) -> V1 + V2 end, Dict, Result),
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
get_dict(Data) ->
	Dict = dict:new(),
	UniqueData = gb_sets:to_list(gb_sets:from_list(Data)), % Professor Arne Andersson's General Balanced Trees, works well with a lot of data
	?DETAIL("UniqueData: ~p\n", [UniqueData]),
	initial_counters(Dict, UniqueData).% returns updated Dict

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
make_sets(Dict, Initial, Size) ->
	case dict:fetch_keys(Dict) of
		[] -> [];
		Keys -> combinations(Keys, Initial, Size, [])
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
combinations([_Elem], _Tails, _Size, Acu) ->
	lists:usort(Acu);
combinations([Elem|Elems], Tails, Size, Acu) ->
	Products = [ lists:usort(Elem++E) || E <- Tails],
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

