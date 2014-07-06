-module(c45).

%%
%% Include files
%%

-include("structures.hrl").
-include("debug.hrl").

%%
%% Exported Functions
%%

-export([learn/4, learn/5, classify/3, print_tree/1]).
%% parallel
-export([build_tree/6, igc_get_entropy/5, igf_get_factors/5]).

%%
%% API Functions
%%

% @doc Function takes the additional supervisor Pid argument and invokes learn/5
learn(Attributes, Class, TrainingExamples, Options, Pid) ->
	Pid ! learn(Attributes, Class, TrainingExamples, Options).	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Decision Tree:                                                               %
% {root, root, Tree}                                                           %
%               /\                                                             %
%           [{Test(Attribute)Name, Test(Attribute)Value, Children}|Brothers]   %
%                                                   /\                         %
%                                               {leaf, Class}                  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% @doc Options: Stop Options: all_in_one_category - all training examples achieving the leaf are in the same category. <br/> Test Choice Criterion : * information_growth_factor_criterion, * information_gain_criterion. Trim Options: * none * pessimistic (default) * {error_reduction, TrimSet}.
learn(Attributes, Class, NumberedExamples, Options) ->
	?LOG("c45 options: ~p~n", [Options]),
	Stop = case lists:keyfind(stop, 1, Options) of
		false -> all_in_one_category; % default
		{stop, StopDecision} -> StopDecision
		end,
	TestChoice = case lists:keyfind(test_choice, 1, Options) of
			false -> information_growth_factor_criterion; % default
			{test_choice, TestChoiceCriterion} -> TestChoiceCriterion
		end,
	Trim = case lists:keyfind(trim, 1, Options) of
			false -> pessimistic; % default
			{trim, TrimCriterion} -> TrimCriterion
		end,

	TrainingExamples = mllib_tools:denumber_tes(NumberedExamples),
	ExamplesNumbers = mllib_tools:get_numbers(NumberedExamples),
	
	Tree = {root, root, build_tree(Attributes, Class, ExamplesNumbers, get_default_category(Class, TrainingExamples), Attributes, 
					[{stop, Stop}, {test_choice, TestChoice}])},

	%print_tree(Tree),	

	?LOG("Trimming: ~p~n", [Trim]),
	TrimmedTree = make_trimming(Attributes, Class, Tree, TrainingExamples, Trim),

	?LOG("After trimming:~n", []),
	%print_tree(TrimmedTree),	

	{ok, #classifier{ algorithm = c45, attributes = Attributes, class = Class, specific_classifier = TrimmedTree}}.


classify(_Classifier=#classifier{ algorithm = c45, attributes = Attributes, class = _Class, specific_classifier = {root, root, Tree}}, Example, _Options) ->
	{ok, find_class(Attributes, Tree, Example)}.
	
%%
%% Local Functions
%%	

build_tree(Attributes, Class, ExamplesNumbers, DefaultCategory, AttributesLeft, Options) ->
	{NumberedExamples, TrainingExamples} = mllib_tools:get_tes(tes_table, ExamplesNumbers),
	{stop, StopDecision} = lists:keyfind(stop, 1, Options),
	{test_choice, ChoiceCriterion} = lists:keyfind(test_choice, 1, Options),
	?LOG("In c45:build_tree/6: start: AttributesLeft: ~p, TrainingExamples: ~p~n======~n", [AttributesLeft, TrainingExamples]),
	case stop_decision(Class, AttributesLeft, TrainingExamples, DefaultCategory, StopDecision) of
		{stop, Category} -> ?LOG("In c45:build_tree/6: stop!: {leaf, ~p}~n======~n", [Category]), {leaf, Category};
		ok -> 
			{CurrentTest, NextAttributes} = choose_attribute(Attributes, Class, TrainingExamples, AttributesLeft, ChoiceCriterion),
			?LOG("In c45:build_tree/6: with: AttributesLeft: ~p, TrainingExamples: ~p~n>>>>~n", [AttributesLeft, TrainingExamples]),
			?LOG("In c45:build_tree/6: Current test: ~p~n==============~n", [CurrentTest]),
			NewDefaultCategory = get_default_category(Class, TrainingExamples),
			
			NumberedExamplesGroups = mllib_tools:group_numberedexamples(NumberedExamples, Attributes, CurrentTest),
			
			TestResults = case CurrentTest of
				{OrderedOrContinuous, SplitValue} -> 
							CurrentAttribute = OrderedOrContinuous,
							[{"<", SplitValue}, {">=", SplitValue}];
				NominalAttribute -> 	CurrentAttribute = NominalAttribute,
							NominalAttribute#attribute.values
			end,
	
			Future = [{ CurrentAttribute#attribute.name, AttributeValue,
			supervisor_manager:compute(build_tree, 
				[Attributes, Class, mllib_tools:get_numbers(mllib_tools:fetch_training_examples_from_group(NumberedExamplesGroups, AttributeValue)), 
					NewDefaultCategory, NextAttributes, Options])   } 
			|| AttributeValue <- TestResults],

			receive_build_tree(Future, [])
	end.

receive_build_tree([], Results) ->
	lists:reverse(Results);
receive_build_tree([{ AttributeName, AttributeValue, Id} | Future], Results) ->
	receive {result, Id, Result} ->
		receive_build_tree(Future, [{ AttributeName, AttributeValue, Result} | Results])
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Test choosing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

choose_attribute(Attributes, Class, TrainingExamples, AttributesLeft, information_gain_criterion) -> 
	choose_attribute_fun(Attributes, Class, TrainingExamples, AttributesLeft, fun information_gain_criterion/4);
choose_attribute(Attributes, Class, TrainingExamples, AttributesLeft, information_growth_factor_criterion) -> 
	choose_attribute_fun(Attributes, Class, TrainingExamples, AttributesLeft, fun information_growth_factor_criterion/4).

choose_attribute_fun(Attributes, Class, TrainingExamples, AttributesLeft, Criterion) -> 
	case Criterion(Attributes, Class, TrainingExamples, AttributesLeft) of
		{OrderedOrContinuous, _SplitValue} = Pair -> { Pair, lists:delete(OrderedOrContinuous, AttributesLeft)};
		NominalAttribute -> { NominalAttribute, lists:delete(NominalAttribute, AttributesLeft)}
	end.

%% just minimize the entropy, information not counted
information_gain_criterion(Attributes, Class, TrainingExamples, AttributesLeft) ->
	?LOG("information_gain_criterion: ~p\n", [node()]),
	Ids = [spawn(c45, igc_get_entropy, [Attributes, Class, TrainingExamples, Attribute, self()])
	|| Attribute <- AttributesLeft],

	Entropies = receive_igc_entropies(Ids, []),

	?LOG("In information_gain_criterion/4: Entropies: ~p~n---~n", [Entropies]),
	ChosenAttributeTest = get_most_information_gain_attribute_name(Entropies, none, 2),
	case ChosenAttributeTest of
		{AttributeName, Value} -> {mllib_tools:get_attribute(Attributes, AttributeName), Value};
		AttributeName          -> mllib_tools:get_attribute(Attributes, AttributeName)
	end.

% @doc collect entropies for information gain criterion
igc_get_entropy(Attributes, Class, TrainingExamples, Attribute, Pid) ->
	Pid ! {result, self(), information:get_entropies(Attributes, Class, TrainingExamples, [Attribute])}.

receive_igc_entropies([], Results) ->
	lists:reverse(lists:flatten(Results));
receive_igc_entropies([Id | Future], Results) ->
	receive {result, Id, Result} ->
		receive_igc_entropies(Future, [Result | Results])
	end.
	
%% minimize the entropy, information not counted
get_most_information_gain_attribute_name([], MinAttributeTest, _Min) ->
	MinAttributeTest;
get_most_information_gain_attribute_name([{NewMinAttributeTest, _Type, Entropy}|Entropies], _MinAttributeName, Min) when Entropy < Min ->
	get_most_information_gain_attribute_name(Entropies, NewMinAttributeTest, Entropy);
get_most_information_gain_attribute_name([_Bigger|Entropies], MinAttributeTest, Min) ->
	get_most_information_gain_attribute_name(Entropies, MinAttributeTest, Min).


%% -----------------------------------------------------------------------------------
information_growth_factor_criterion(Attributes, Class, TrainingExamples, AttributesLeft) ->
	?LOG("information_growth_factor_criterion: ~p\n", [node()]),
	Ids = [spawn(node(), c45, igf_get_factors, [Attributes, Class, TrainingExamples, Attribute, self()])
	|| Attribute <- AttributesLeft],
	InformationGrowthFactors = receive_igf_factors(Ids, []),
	?LOG("---~n~p~n---~n", [InformationGrowthFactors]),
	ChosenAttributeTest = get_most_information_growth_factor_attribute_name(InformationGrowthFactors, none, -1),
	case ChosenAttributeTest of
		{AttributeName, Value} -> {mllib_tools:get_attribute(Attributes, AttributeName), Value};
		AttributeName          -> mllib_tools:get_attribute(Attributes, AttributeName)
	end.

% @doc collect factors for information growth factor
igf_get_factors(Attributes, Class, TrainingExamples, Attribute, Pid) ->
	Pid ! {result, self(), information:get_information_growth_factors(Attributes, Class, TrainingExamples, [Attribute])}.

receive_igf_factors([], Results) ->
	lists:flatten(Results);
receive_igf_factors([Id | Future], Results) ->
	receive {result, Id, Result} ->
		receive_igf_factors(Future, [Result | Results])
	end.


get_most_information_growth_factor_attribute_name([], MaxAttributeName, _Max) ->
	MaxAttributeName;
get_most_information_growth_factor_attribute_name([{NewMaxAttributeName, IGF} | InformationGrowthFactors], _MaxAttributeName, Max) when IGF > Max ->
	get_most_information_growth_factor_attribute_name(InformationGrowthFactors, NewMaxAttributeName, IGF);
get_most_information_growth_factor_attribute_name([_Lower|InformationGrowthFactors], MaxAttributeName, Max) ->
	get_most_information_growth_factor_attribute_name(InformationGrowthFactors, MaxAttributeName, Max).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Stops
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_default_category(Class, TrainingExamples) ->
	CategoryCounters = mllib_tools:get_trainingexamples_categories_counters(Class#class.categories, TrainingExamples),
	choose_maxcount_category(CategoryCounters, -1, none).

choose_maxcount_category([], _MaxCount, MaxCategory) ->
	MaxCategory;
choose_maxcount_category([{Category, N}|CategoryCounters], MaxCount, _MaxCategory) when N>MaxCount ->
	choose_maxcount_category(CategoryCounters, N, Category);
choose_maxcount_category([_NotMax|CategoryCounters], MaxCount, MaxCategory) ->
	choose_maxcount_category(CategoryCounters, MaxCount, MaxCategory).

%%% Default:
%%% case Tests are_empty return default_category, // currently Tests = My_attribs
%%% case TrainingExamples are_empty return default_category
%%% case only_one_category(TrainingExamples) return the_only_category
% all_in_one_category(Class, Tests, TrainingExamples, DefaultCategory)	
all_in_one_category([] = _Tests, _TrainingExamples, DefaultCategory) ->
	{stop, DefaultCategory};
all_in_one_category(_Tests, [] = _TrainingExamples, DefaultCategory) ->
	{stop, DefaultCategory};
all_in_one_category(_Tests, TrainingExamples, _DefaultCategory) ->
	case only_one_category(TrainingExamples) of
		{ok, Category} -> {stop, Category};
		_ -> ok
	end.

only_one_category([{_DataAtribs, Category}|TrainingExamples]) ->
	only_one_category(TrainingExamples, Category).

only_one_category([], OnlyCategory) ->
	{ok, OnlyCategory};
only_one_category([{_DataAtribs, OnlyCategory}|TrainingExamples], OnlyCategory) ->
	only_one_category(TrainingExamples, OnlyCategory);
only_one_category([_NotReally|_TrainingExamples], _OnlyCategory) ->
	{error, multiple_categories}.

stop_decision(_Class, Tests, TrainingExamples, DefaultCategory, all_in_one_category) -> 
	all_in_one_category(Tests, TrainingExamples, DefaultCategory).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Trimming funs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Można określić procent zbioru trenujacego uzytego do strzyzenia, albo podac stricte zbior trenujacy i zbior do przycinania

%%%%%%%%%%%%%%%%%%%%%
% args:
% Tree - tree to be trimmed
% TrimSet - TrainingExamples used to determine the quality of trimming
%             (is used to determine whether to trim the node or not)
% returns:
% TrimmedTree
%%%%%%%%%%%%%%%%%%%%%
make_trimming(_Attributes, _Class, Tree, _TrimSet, none = _Trim) ->
	Tree;
make_trimming(Attributes, Class, Tree, TrimSet, pessimistic = _Trim) ->
	trim(Attributes, Class, fun pessimistic_trim/4, Tree, TrimSet);
make_trimming(Attributes, Class, Tree, _TrimSet, {error_reduction, AlternativeTrimSet = _Trim}) ->
	trim(Attributes, Class, fun hypothesis_error_rate/4, Tree, AlternativeTrimSet).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% fun error:                                                           %
% function classifies the given TrainingExamples using Tree            %
% and then compares the result with an expected Category               %
% It returns a comparable value, the higher value the bigger the error %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
trim(Attributes, Class, CountError, {root, root, Children} = _Tree, TrimSet) ->
	{root, root, trim(Attributes, Class, CountError, Children, TrimSet)};
trim(_Attributes, _Class, _CountError, {leaf, _Category} = Leaf, _TrimSet) ->
	Leaf;
trim(_Attributes, _Class, _CountError, [] = _Nodes, _TrimSet) ->
	[];
trim(Attributes, Class, CountError, Node, TrimSet) when is_list(Node)->
	Leaf = {leaf, get_default_category(Class, TrimSet)},
	CurrentError = CountError(Attributes, Class, Node, TrimSet),
	TrimmedError = CountError(Attributes, Class, Leaf, TrimSet),
	if
		CurrentError > TrimmedError -> Leaf;
		true                        -> [{AttributeName, AttributeValue, trim(Attributes, Class, CountError, Children, 							mllib_tools:filter_training_examples_using_test(Attributes, AttributeName, AttributeValue, TrimSet))}
					        || {AttributeName, AttributeValue, Children} <- Node]
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% for leaf = error_rate (e{c,p})                             %
%%% for node = e{c,p} + sqrt((e{c,p} * (1 - e{c, p})) / |P|)   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
pessimistic_trim(Attributes, Class, {leaf, _Category} = Leaf, TrainingExamples) ->
	error_rate(Attributes, Class, Leaf, TrainingExamples);
pessimistic_trim(Attributes, Class, Node, TrainingExamples) ->
	ErrorRate = error_rate(Attributes, Class, Node, TrainingExamples),
	ErrorRate + math:sqrt( (ErrorRate*(1-ErrorRate)) / length(TrainingExamples) ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% hypothesis error rate %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% r{c,p} = |{x in P | h(x) != c(x)}| %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
hypothesis_error_rate(Attributes, _Class, Tree, TrainingExamples) ->
	hypothesis_error_rate_acu(Attributes, Tree, TrainingExamples, 0).

hypothesis_error_rate_acu(_Attributes, _Tree, [] = _TrainingExamples, Acu) ->
	Acu;
hypothesis_error_rate_acu(Attributes, Tree, [{Example, ExpectedCategory}=_TrainingExample|TrainingExamples], Acu) ->
	case find_class(Attributes, Tree, Example) == ExpectedCategory of
		false -> hypothesis_error_rate_acu(Attributes, Tree, TrainingExamples, Acu+1);
		true -> hypothesis_error_rate_acu(Attributes, Tree, TrainingExamples, Acu)
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% error rate %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% e{c,p} = r{c,p} / |P| %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% where r{c,p} = |{x in P | h(x) != c(x)}| = hypothesis error rate %%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
error_rate(Attributes, Class, Tree, TrainingExamples) ->
	hypothesis_error_rate(Attributes, Class, Tree, TrainingExamples) / length(TrainingExamples).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Classify funs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
find_class(Attributes, [{AttributeName, {"<", TestValue}, NextLower}, {AttributeName, {">=", TestValue}, NextGreaterEq}], Example) ->
	CurrentValue = mllib_tools:get_attribute_value(Attributes, Example, AttributeName),
	Attribute = mllib_tools:get_attribute(Attributes, AttributeName),
	case mllib_tools:compare(Attribute, CurrentValue, TestValue) of
		lower           -> find_class(Attributes, NextLower, Example);
		_GreaterOrEqual -> find_class(Attributes, NextGreaterEq, Example)
	end;
find_class(Attributes, [{AttributeName, AttributeValue, NextAttributes}|NextAttributeValue], Example) ->
	case mllib_tools:get_attribute_value(Attributes, Example, AttributeName) == AttributeValue of
		true -> find_class(Attributes, NextAttributes, Example);
		false -> find_class(Attributes, NextAttributeValue, Example)
	end;
find_class(_Attributes, {leaf, Class}, _Example) -> Class.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% DEBUG printing funs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% @doc fancy print the ready classifier
print_tree({root, _Value, Children}) ->
	io:format("~n========PRINT TREE=========~n~n"),
	fancy_print_tree(Children, 1),
	io:format("~n=============================~n").


fancy_print_tree([], Indent) -> 
	make_indent(Indent),
	io:format("<<<<<<<<<<~n");
fancy_print_tree([{Node, Value, Children}|T], Indent) ->
	make_indent(Indent),
	io:format("~p: ~p~n", [Node, Value]),

	fancy_print_tree(Children, Indent+1),

	fancy_print_tree(T, Indent);
fancy_print_tree({leaf,Val}, Indent) ->
	make_indent(Indent),
	io:format("*-*-*-*-* Leaf: ~p *-*-*-*-*~n", [Val]).

make_indent(0) ->
	ok;
make_indent(N) ->
	io:format("\t"),
	make_indent(N-1).
	

