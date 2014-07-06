-module(c45single).

%%
%% Include files
%%

-include("structures.hrl").
-include("debug.hrl").

%%
%% Exported Functions
%%

-export([learn/4, learn/5, classify/3]).

%%
%% API Functions
%%

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
% @doc Options: Stop Options: all_in_one_category - all training examples achieving the leaf are in the same category. <br/> Test Choice Criterium : * information_growth_factor_criterium, * information_gain_criterium. Trim Options: * none * pesimistic (default) * {error_reduction, TrimSet}.
learn(Attributes, Class, TrainingExamples, Options) ->
	?LOG("c45 options: ~p~n", [Options]),
	Stop = case lists:keyfind(stop, 1, Options) of
		false -> all_in_one_category; % default
		{stop, StopDecision} -> StopDecision
		end,
	TestChoice = case lists:keyfind(test_choice, 1, Options) of
			false -> information_growth_factor_criterium; % default
			{test_choice, TestChoiceCriterium} -> TestChoiceCriterium
		end,
	Trim = case lists:keyfind(trim, 1, Options) of
			false -> pesimistic; % default
			{trim, TrimCriterium} -> TrimCriterium
		end,

	Tree = {root, root, build_tree(Attributes, Class, TrainingExamples, get_default_category(Class, TrainingExamples), Attributes, 
					[{stop, Stop}, {test_choice, TestChoice}])},

	print_tree(Tree),	

	?LOG("Trimming: ~p~n", [Trim]),
	TrimmedTree = make_trimming(Attributes, Class, Tree, TrainingExamples, Trim),

	?LOG("After trimming:~n", []),
	print_tree(TrimmedTree),	

	{ok, #classifier{ algorithm = c45, attributes = Attributes, class = Class, specific_classifier = TrimmedTree}}.


classify(_Classifier=#classifier{ algorithm = c45, attributes = Attributes, class = _Class, specific_classifier = {root, root, Tree}}, Example, _Options) ->
	{ok, find_class(Attributes, Tree, Example)}.
	
%%
%% Local Functions
%%	

build_tree(Attributes, Class, TrainingExamples, DefaultCategory, AttributesLeft, Options) ->
	{stop, StopDecision} = lists:keyfind(stop, 1, Options),
	{test_choice, ChoiceCriterium} = lists:keyfind(test_choice, 1, Options),
	?LOG("In c45:build_tree/6: start: AttributesLeft: ~p, TrainingExamples: ~p~n======~n", [AttributesLeft, TrainingExamples]),
	case stop_decision(Class, AttributesLeft, TrainingExamples, DefaultCategory, StopDecision) of
		{stop, Category} -> ?LOG("In c45:build_tree/6: stop!: {leaf, ~p}~n======~n", [Category]), {leaf, Category};
		ok -> 
			{CurrentTest, NextAttributes} = choose_attribute(Attributes, Class, TrainingExamples, AttributesLeft, ChoiceCriterium),
			?LOG("In c45:build_tree/6: with: AttributesLeft: ~p, TrainingExamples: ~p~n>>>>~n", [AttributesLeft, TrainingExamples]),
			?LOG("In c45:build_tree/6: Current test: ~p~n==============~n", [CurrentTest]),
			NewDefaultCategory = get_default_category(Class, TrainingExamples),
			
			TrainingExamplesGroups = mllib_tools:group_trainingexamples(TrainingExamples, Attributes, CurrentTest),
			
			TestResults = case CurrentTest of
				{OrderedOrContinuous, SplitValue} -> 
							CurrentAttribute = OrderedOrContinuous,
							[{"<", SplitValue}, {">=", SplitValue}];
				NominalAttribute -> 	CurrentAttribute = NominalAttribute,
							NominalAttribute#attribute.values
			end,
				
			
			[{ CurrentAttribute#attribute.name, AttributeValue,
			build_tree(Attributes, Class, mllib_tools:fetch_training_examples_from_group(TrainingExamplesGroups, AttributeValue), 
					NewDefaultCategory, NextAttributes, Options)} 
			|| AttributeValue <- TestResults]
	end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Test choosing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

choose_attribute(Attributes, Class, TrainingExamples, AttributesLeft, information_gain_criterium) -> 
	choose_attribute_fun(Attributes, Class, TrainingExamples, AttributesLeft, fun information_gain_criterium/4);
choose_attribute(Attributes, Class, TrainingExamples, AttributesLeft, information_growth_factor_criterium) -> 
	choose_attribute_fun(Attributes, Class, TrainingExamples, AttributesLeft, fun information_growth_factor_criterium/4).

choose_attribute_fun(Attributes, Class, TrainingExamples, AttributesLeft, Criterium) -> 
	case Criterium(Attributes, Class, TrainingExamples, AttributesLeft) of
		{OrderedOrContinuous, _SplitValue} = Pair -> { Pair, lists:delete(OrderedOrContinuous, AttributesLeft)};
		NominalAttribute -> { NominalAttribute, lists:delete(NominalAttribute, AttributesLeft)}
	end.

%% just minimize the entropy, information not counted
information_gain_criterium(Attributes, Class, TrainingExamples, AttributesLeft) ->
	Entropies = information:get_entropies(Attributes, Class, TrainingExamples, AttributesLeft),

	?LOG("In information_gain_criterium/4: Entropies: ~p~n---~n", [Entropies]),
	ChosenAttributeTest = get_most_information_gain_attribute_name(Entropies, none, 2),
	case ChosenAttributeTest of
		{AttributeName, Value} -> {mllib_tools:get_attribute(Attributes, AttributeName), Value};
		AttributeName          -> mllib_tools:get_attribute(Attributes, AttributeName)
	end.
	
%% minimize the entropy, information not counted
get_most_information_gain_attribute_name([], MinAttributeTest, _Min) ->
	MinAttributeTest;
get_most_information_gain_attribute_name([{NewMinAttributeTest, _Type, Entropy}|Entropies], _MinAttributeName, Min) when Entropy < Min ->
	get_most_information_gain_attribute_name(Entropies, NewMinAttributeTest, Entropy);
get_most_information_gain_attribute_name([_Bigger|Entropies], MinAttributeTest, Min) ->
	get_most_information_gain_attribute_name(Entropies, MinAttributeTest, Min).


%% -----------------------------------------------------------------------------------
information_growth_factor_criterium(Attributes, Class, TrainingExamples, AttributesLeft) ->
	InformationGrowthFactors = information:get_information_growth_factors(Attributes, Class, TrainingExamples, AttributesLeft),
	?LOG("---~n~p~n---~n", [InformationGrowthFactors]),
	ChosenAttributeTest = get_most_information_growth_factor_attribute_name(InformationGrowthFactors, none, -1),
	case ChosenAttributeTest of
		{AttributeName, Value} -> {mllib_tools:get_attribute(Attributes, AttributeName), Value};
		AttributeName          -> mllib_tools:get_attribute(Attributes, AttributeName)
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
make_trimming(Attributes, Class, Tree, TrimSet, pesimistic = _Trim) ->
	trim(Attributes, Class, fun pesimistic_trim/4, Tree, TrimSet);
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
pesimistic_trim(Attributes, Class, {leaf, _Category} = Leaf, TrainingExamples) ->
	error_rate(Attributes, Class, Leaf, TrainingExamples);
pesimistic_trim(Attributes, Class, Node, TrainingExamples) ->
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
	

