%%
%% Naive Bayes
%%

-module(naive_bayes).

%%
%% Include files
%%

-include("structures.hrl").
-include("debug.hrl").

%%
%% Exported Functions
%%

-export([learn/4, classify/3]).
-export([computeConditionalProbContinuous/4, computeConditionalProbValueLoop/5]).

%%
%% Internal records/types
%%

-record(nbClassifier, {categoryProb = dict:new(), condProb = dict:new()}).

%%
%% API Functions
%%

learn(Attributes, Class, NumberedExamples, _Options) ->
	TrainingExamples = mllib_tools:denumber_tes(NumberedExamples),
	NBClassifier = #nbClassifier{},
	NBClassifierCategories = computeCategoriesProb(NBClassifier, TrainingExamples),
	?LOG( "Categories \"probabilities\": ~n", []),
	print_categories_prob(NBClassifierCategories#nbClassifier.categoryProb),
	?LOG("Conditional probabilities:~n", []),
	NBClassifierCategoriesAndConditional = computeConditionalProb(NBClassifierCategories, Attributes),
	{ok, #classifier{ algorithm = naive_bayes, attributes = Attributes, class = Class, specific_classifier = NBClassifierCategoriesAndConditional }}.


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

% Compute categories probabilities (number of occurrences in TrainingExamples)
computeCategoriesProb(NBClassifier, TrainingExamples) ->
	countCategories(NBClassifier, TrainingExamples).

% Count occurrences of each category in TrainingExamples
countCategories(NBClassifier, [] = _TrainingExamples) -> NBClassifier;
countCategories(NBClassifier, TrainingExamples) ->
	[{_, CategoryName} | Tail] = TrainingExamples,
	{_, CategoriesProb, AttribsProb} = NBClassifier,
	FindResult = dict:find(CategoryName, CategoriesProb),
	case FindResult of
		error -> countCategories({nbClassifier, dict:store(CategoryName, 1.0, CategoriesProb), AttribsProb}, Tail);
		{ok, Value} -> countCategories({nbClassifier, dict:store(CategoryName, Value + 1.0, CategoriesProb), AttribsProb}, Tail)
	end.


% Compute conditional probabilities
computeConditionalProb(NBClassifier, Attributes) ->
	Workers = computeConditionalProbCategoryLoop(NBClassifier, dict:to_list(NBClassifier#nbClassifier.categoryProb), Attributes, []),
	receive_results(Workers, NBClassifier).

receive_results([] = _Workers, NBClassifier) ->
	NBClassifier;
receive_results([Worker|Workers], _NBClassifier = #nbClassifier{categoryProb = CategoryProb, condProb = CondProb}) ->
	receive {result, Worker, _NBClassifierPart = #nbClassifier{condProb = CondProbPart}} ->
		NBClassifierNew = #nbClassifier{categoryProb = CategoryProb, condProb = dict:merge(fun (_K, V1, _V2) -> V1 end, CondProb, CondProbPart)},
		receive_results(Workers, NBClassifierNew)
	end.

% Traverse over CategoryList
computeConditionalProbCategoryLoop(_NBClassifier, [] = _CategoryList, _Attributes, Workers) -> 
	Workers;
computeConditionalProbCategoryLoop(NBClassifier, [Category | Tail] = _CategoryList, Attributes, Workers) ->
	?LOG("  category ~w ~n", [Category]),
	NewWorkers = computeConditionalProbAttribLoop(NBClassifier, Category, Attributes, 1, Workers),
	computeConditionalProbCategoryLoop(NBClassifier, Tail, Attributes, NewWorkers).

% Traverse over AttribList
computeConditionalProbAttribLoop(_NBClassifier, _Category, [] = _AttribList, _AttribIndex, Workers) -> 
	Workers;
computeConditionalProbAttribLoop(NBClassifier, Category, [Attribute | Tail] = _AttribList, AttribIndex, Workers) ->
	?LOG("    attrib ~w ~n", [Attribute#attribute.name]),
	case Attribute#attribute.type of
		continuous ->
			NewWorker = supervisor_manager:compute(computeConditionalProbContinuous,
					[NBClassifier, Category, Attribute, AttribIndex]),
			computeConditionalProbAttribLoop(NBClassifier, Category, Tail, AttribIndex+1, [NewWorker|Workers]);
		_Other ->
			NewWorker = supervisor_manager:compute(computeConditionalProbValueLoop, 
					[NBClassifier, Category, Attribute, AttribIndex, Attribute#attribute.values]),
			computeConditionalProbAttribLoop(NBClassifier, Category, Tail, AttribIndex+1, [NewWorker|Workers])
	end.

computeConditionalProbValueLoop(NBClassifier, Category, Attribute, AttribIndex, ValuesList) ->
	TrainingExamples = mllib_tools:get_all_tes(tes_table),
	computeConditionalProbValueLoop(NBClassifier, TrainingExamples, Category, Attribute, AttribIndex, ValuesList).	

% Traverse over Attribute.values
computeConditionalProbValueLoop(NBClassifier, _TrainingExamples, _Category, _Attribute, _AttribIndex, [] = _ValuesList) -> NBClassifier;
computeConditionalProbValueLoop(NBClassifier, TrainingExamples, Category, Attribute, AttribIndex, ValuesList) ->
	[Value | Tail] = ValuesList,
	?LOG("      value ~w ", [Value]),
	NBClassifierNew = computeConditionalProb(NBClassifier, TrainingExamples, Category, Attribute, AttribIndex, Value),
	computeConditionalProbValueLoop(NBClassifierNew, TrainingExamples, Category, Attribute, AttribIndex, Tail).

% For given Category, Attribute and Value compute conditional probability
computeConditionalProb(NBClassifier, TrainingExamples, {CategoryName, CategoryProb}, Attribute, AttribIndex, Value) ->
	Nominator = lists:foldl(
		fun(TrainingExample, Acc) ->
			{TEValue, TECategory} = TrainingExample,
			if
				(element(AttribIndex, TEValue) == Value) and (TECategory == CategoryName) -> Acc + 1.0;
				true -> Acc
			end
			end,
		1.0,
		TrainingExamples),
	Denominator = CategoryProb + length( Attribute#attribute.values ),
	Probability = Nominator / Denominator,
	?LOG(" ~w~n", [Probability]),
	CategoryProbDict = NBClassifier#nbClassifier.categoryProb,
	CondProbDict = NBClassifier#nbClassifier.condProb,
	#nbClassifier{categoryProb = CategoryProbDict, condProb = dict:store({CategoryName, Attribute, Value}, Probability, CondProbDict)}.


% For given Category and Attribute traverse through TrainingExamples and compute mean and variance
computeConditionalProbContinuous(NBClassifier, {CategoryName, _}, Attribute, AttribIndex) ->
	TrainingExamples = mllib_tools:get_all_tes(tes_table),
	{Sum, Denominator} = lists:foldl(
		fun(TrainingExample, {AccSum, AccDenom}) ->
			{TEValue, TECategory} = TrainingExample,
			if
				(TECategory == CategoryName) -> {AccSum + element(AttribIndex, TEValue), AccDenom + 1.0};
				true -> {AccSum, AccDenom}
			end
			end,
		{0.0, 0.0},
		TrainingExamples),
	Mean = Sum / Denominator,
	SumVar = lists:foldl(
		fun(TrainingExample, Acc) ->
			{TEValue, TECategory} = TrainingExample,
			if
				(TECategory == CategoryName) -> Acc + math:pow((element(AttribIndex, TEValue) - Mean), 2);
				true -> Acc
			end
			end, 0.0, TrainingExamples),
	Variance = SumVar / max(1.0, (Denominator - 1.0)),
	CategoryProbDict = NBClassifier#nbClassifier.categoryProb,
	CondProbDict = NBClassifier#nbClassifier.condProb,
	#nbClassifier{categoryProb = CategoryProbDict, condProb = dict:store({CategoryName, Attribute}, {Mean, Variance}, CondProbDict)}.


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
