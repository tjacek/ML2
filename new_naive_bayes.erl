%%
%% Naive Bayes
%%

-module(new_naive_bayes).

%%
%% Include files
%%

-include("structures.hrl").
-include("debug.hrl").

%%
%% Exported Functions
%%

-export([learn/4, classify/3]).
-export([naive_worker/2]).

%%
%% Internal records/types
%%

-record(nbClassifier, {categoryProb = dict:new(), condProb = dict:new()}).

%%
%% API Functions
%%

learn(Attributes, Class, TrainingExamples, Options) ->
	WorkersNo = case lists:keyfind(workers, 1, Options) of
		false -> 1; % default
		{workers, Workers} -> Workers
		end,
	NBClassifierCategoriesAndConditional = computeConditionalProb(TrainingExamples, Class, Attributes, WorkersNo),
	{ok, NBClassifierCategoriesAndConditional}.

classify(Classifier, Example, _Options) ->
	?LOG("Classifying ~w...~n", [Example]),
	#classifier{attributes = Attributes, specific_classifier = NBClassifier} = Classifier,
	#nbClassifier{categoryProb = CategoryProb, condProb = CondProb} = NBClassifier,
	Probabilities = lists:map( fun(Category) -> classifyComputeProbability(Category, Example, Attributes, CondProb) end, dict:to_list(CategoryProb) ),
	?LOG("    Probabilities: ~w~n", [Probabilities]),
	{_, Result} = lists:max([ {B, A} || {A, B} <- Probabilities]),
	?LOG("    Maximum: ~w~n", [Result]),
	{ok, Result}.
	
	
%%
%% Local Functions
%%	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% WORKERS SPAWN HERE <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<!
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Compute conditional probabilities
computeConditionalProb(TrainingExamples, Class, Attributes, WorkersNo) ->
	Workers = start_workers(TrainingExamples, Class, Attributes, WorkersNo),
	PartialResults = receive_results(Workers, {[],[],[]}),
	reduce(PartialResults).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
%%%%%%%%%%%%%%%%%%%%
%%%%%%%%

start_workers(TEs, Class, Attributes, WorkersNo) ->
	SplittedTEs = split_data(TEs, WorkersNo), % split Data into WorkersNo parts
	WorkersIds = [ supervisor_manager:compute(naive_worker, [SData, Attributes]) || SData <- SplittedTEs ], % send data to workers
	WorkersIds.

receive_results([] = _Workers, ResultsAcu) ->
	ResultsAcu;
receive_results([Worker|Workers], {ResultsAcuNominal, ResultsAcuContinuous, ResultsAcuApriori}) ->
	receive {result, Worker, {ResultsNominal, ResultsContinuous, ResultsApriori}} ->
		receive_results(Workers,
			{[ResultsNominal|ResultsAcuNominal], [ResultsContinuous|ResultsAcuContinuous], [ResultsApriori|ResultsAcuApriori]})
	end.


naive_worker(TEs, Attributes) ->
	process_flag(trap_exit, true),
	?LOG("Before open\n", []),

	Port = open_port({spawn, "port/naive_worker"}, [{packet, 4}, binary]),
	?LOG("Port openned ~p\n", [Port]),

	Port ! {self(), {command, term_to_binary([byte_type(Att#attribute.type) || Att <- Attributes])}},
	?LOG("Attributes sent\n", []),

	Port ! {self(), {command, term_to_binary(TEs)}},
	?LOG("Trainingexamples sent\n", []),

	receive
		{Port, {data, Data}} ->
			?LOG("Got data from port\n", []),
			Port ! {self(), close},
			?LOG("Close sent\n", []),
			receive
				{Port, closed} ->
					{PartialNominal, PartialContinuous, PartialApriori} = binary_to_term(Data),
					?LOG("Port close ACK\n Data:\n~p\n", [binary_to_term(Data)]),
					{dict:from_list(PartialNominal), dict:from_list(PartialContinuous), dict:from_list(PartialApriori)}
			end;
		{'EXIT', Port, Reason} ->
			?LOG("'Exit' Port down : ~p\n", [Reason]),
			exit(port_terminated)
	end.

byte_type(nominal) ->
	0;
byte_type(ordered) ->
	0;
byte_type(continuous) ->
	1 .

inspectTes([]=_TEs, _Attributes, Dict) ->
	Dict;
inspectTes([Te|TEs], Attributes, Dict) ->
	inspectTes(TEs, Attributes, inspectTEAtts(Te, Attributes, length(Attributes), Dict)).

inspectTEAtts(_Te, _Attributes, 0, Dict) ->
	Dict;
inspectTEAtts(Te, [Att|Attributes], N, Dict) ->
	{_E, Category} = Te,
	Value = mllib_tools:get_n_attribute_value(Te, N),
	Key = {Category, Att#attribute.name, Value},
	inspectTEAtts(Te, Attributes, N-1, dict:update(Key, fun (Old) -> Old + 1 end, 1, Dict)).

reduce({ResultsN, ResultsC, ResultsA}) ->
	{ dict:to_list(merge_dicts(ResultsN, dict:new())),
		dict:to_list(merge_dicts_cont(ResultsC, dict:new())),
		dict:to_list(merge_dicts(ResultsA, dict:new())) }.

merge_dicts([] = _Results, Dict) ->
	Dict;
merge_dicts([Res|Results], Dict) ->
	merge_dicts(Results, dict:merge(fun (_K, V1, V2) -> V1 + V2 end, Dict, Res)).
merge_dicts_cont([Res|Results], Dict) ->
	merge_dicts(Results, dict:merge(fun (_K, {V1a, V1b}, {V2a, V2b}) -> {V1a + V2a, V1b + V2b} end, Dict, Res)).

	

%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%
%%%%%% CLASSIFY %%%%%%
%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%

% For given Category and Example compute product of probabilities
classifyComputeProbability(Category, Example, Attributes, CondProb) ->
	{CategoryName, CategoryProb} = Category,
	CategoryAttrValList = lists:zipwith(fun(X, Y) -> {CategoryName, X, Y} end, Attributes, tuple_to_list(Example)),
	Probability = lists:foldl( fun(Key, Acc) ->
		{_, Attrib, Value} = Key,
		if
			(Attrib#attribute.type == continuous) -> Acc * gaussianFunc(dict:fetch({CategoryName, Attrib}, CondProb), Value);
			true -> Acc * dict:fetch(Key, CondProb)
		end
		end, CategoryProb+1.0, CategoryAttrValList ),
	{CategoryName, Probability}.

% For given parameters and argument compute value of Gaussian function
gaussianFunc({Mean, Variance}, X) ->
	A = 1 / math:sqrt(2.0 * math:pi() * Variance),
	Value = A * math:exp( - math:pow((X - Mean), 2.0) / (2.0 * Variance) ),
	?LOG("    gaussianFunc(~w, ~w, ~w) = ~w~n", [Mean, Variance, X, Value]),
	Value.

%%
%% Extra Functions
%%

print_categories_prob(Dict) ->
	dict:map(
	fun(K,V) -> 
		?LOG("    P( category = ~w ) = ~w~n", [K, V]) end,
	Dict).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
split_data(Data, Workers) ->
	Dict = dict:new(),
	split_workers_acu(Data, Workers, 1, Dict).
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
split_workers_acu([], _Workers, _N, Dict) ->
	[ List || {_No, List} <- dict:to_list(Dict)];
split_workers_acu([Elem|Data], Workers, Workers, Dict) ->
	split_workers_acu(Data, Workers, 1, append_to_dict(Workers, Elem, Dict));
split_workers_acu([Elem|Data], Workers, N, Dict) ->
	split_workers_acu(Data, Workers, N+1, append_to_dict(N, Elem, Dict)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
append_to_dict(Key, Value, Dict) ->
	dict:update(Key, fun (Old) -> [Value|Old] end, [Value], Dict).

