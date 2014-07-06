-module(mllib_tools).

%%
%% Include files
%%
-include("structures.hrl").
-include("debug.hrl").

%%
%% Exported Functions
%%

-export([get_attribute_value/3, get_n_attribute_value/2, get_attribute_pos/2]).
-export([set_attribute_value/4]).
-export([create_training_examples_groups/2, add_training_example_to_group/2,
		fetch_training_examples_from_group/2, group_trainingexamples/2,
		group_comparable_trainingexamples/3, group_trainingexamples/3]).
-export([create_counter_foreach_category/1, count_trainingexamples_categories/2,
			get_trainingexamples_categories_counters/2]).
-export([filter_training_examples_using_test/4]).
-export([split_trainingexamples/4]).
-export([get_attribute/2, get_n_attribute/2]).
-export([get_attribute_nth_value/3, get_attribute_nth_value/2, get_attribute_value_pos/2]).
-export([exclude/2, exclude_multi/2]).
-export([compare/2, compare/3, sort_trainingexamples/3]).
-export([denumber_tes/1]).
-export([group_numberedexamples/3, group_comparable_numberedexamples/3,
	group_numberedexamples/2, add_numbered_example_to_group/2]).
-export([get_numbers/1, get_tes/2, get_tes_data/2, get_all_tes/1]).
-export([compute_error/3]).

%%
%% API Functions
%%


%%%%%%%%
%%%%%%%% TrainingExamples API
%%%%%%%%

-spec get_attribute_value(Attributes :: [attribute()], Example :: training_example() | example(), AttributeName :: name()) -> Value :: value().
% @spec get_attribute_value(Attributes, Example, AttributeName) -> Value where Attributes = [attribute()], Example = example() | training_example(), AttributeName = name(), Value = value()
% @doc Get AttributeName Value from TrainingExample or Example, given Attributes definition. <br/><b>Note:</b> This should be used for a single TrainingExample or Example value checking only. For operations on the list of Examples(TrainingExamples) calling get_attribute_pos/2 and then get_n_attribute_value/2 is adviced.
get_attribute_value(Attributes, {Example , _class} = _TrainingExample, AttributeName) when is_tuple(Example) ->
	erlang:element(get_attribute_pos(Attributes, AttributeName, 1), Example);
get_attribute_value(Attributes, Example, AttributeName) ->
	erlang:element(get_attribute_pos(Attributes, AttributeName, 1), Example).

%%---------------------------------------------------------------------------------------

% @doc Set AttributeName Value in TrainingExample or Example, given Attributes definition. <br/><b>Note:</b> This should be used for a single TrainingExample or Example value checking only. For operations on the list of Examples(TrainingExamples) calling get_attribute_pos/2 and then set_n_attribute_value/2 is adviced.
set_attribute_value(Attributes, {Example , Class} = _TrainingExample, AttributeName, Value) when is_tuple(Example) ->
	{set_attribute_value(Attributes, Example, AttributeName, Value), Class};
set_attribute_value(Attributes, Example, AttributeName, Value) ->
	Pos = get_attribute_pos(Attributes, AttributeName, 1),	
	List = replace_nth(tuple_to_list(Example), Pos, Value, 1),
	list_to_tuple(List).
	
%%---------------------------------------------------------------------------------------

-spec get_n_attribute_value(Example :: training_example() | example(), AttributePos :: integer()) -> Value :: value().
% @spec get_n_attribute_value(Example, AttributePos) -> Value where Example = example() | training_example(), AttributePos = integer(), Value = value()
% @doc Get Value of attribute located on the AttributePos position from Example or TrainingExample, where AttributePos is the position obtained from get_attribute_pos/2.
get_n_attribute_value({Example , _class} = _TrainingExample, AttributePos) when is_tuple(Example) ->
	erlang:element(AttributePos, Example);
get_n_attribute_value(Example, AttributePos) ->
	erlang:element(AttributePos, Example).

%%---------------------------------------------------------------------------------------

% @doc Get N-th Attribute Value , useful especially with oredered
get_attribute_nth_value(Attributes, AttributeName, N) ->
	Attribute = get_attribute(Attributes, AttributeName),
	lists:nth(N, Attribute#attribute.values).

% @doc Get N-th Attribute Value , useful especially with oredered
get_attribute_nth_value(#attribute{} = Attribute, N) ->
	lists:nth(N, Attribute#attribute.values).

% @doc Get Pos of Value in Attribute , useful with oredered
get_attribute_value_pos(Attribute, Value) ->
	get_pos(Attribute#attribute.values, Value, 1).


%%---------------------------------------------------------------------------------------

-spec get_attribute_pos(Attributes :: [attribute()], AttributeName :: name()) -> Position :: integer().
% @spec get_attribute_pos(Attributes, AttributeName) -> Position where Attributes = [attribute()], AttributeName = name(), Position = integer()
% @doc Get the position of the AttributeName in the Example tuple given the the Attributes definition.
get_attribute_pos(Attributes, #attribute{name = AttributeName} = _Attribute) ->
	?DETAIL("In get_attribute_pos/2: Attributes: ~p~nAttributeName: ~p~n--~n",
		[Attributes, AttributeName]),
	get_attribute_pos(Attributes, AttributeName, 1);
get_attribute_pos(Attributes, AttributeName) ->
	?DETAIL("In get_attribute_pos/2: Attributes: ~p~nAttributeName: ~p~n--~n",
		[Attributes, AttributeName]),
	get_attribute_pos(Attributes, AttributeName, 1).
%%---------------------------------------------------------------------------------------

-spec filter_training_examples_using_test(Attributes :: [attribute()], AttributeName :: name(), AttributeValue :: any(), TrainingExamples :: [training_example()]) -> FilteredTrainingExamples :: [training_example()].
% @spec filter_training_examples_using_test(Attributes, AttributeName, AttributeValue, TrainingExamples) -> FilteredTrainingExamples where Attributes = [attribute()], AttributeName = name(), TrainingExamples = [training_example()], FilteredTrainingExamples = [training_example()]
% @doc Returns the list of training examples that satisfy the given test.
filter_training_examples_using_test(Attributes, AttributeName, AttributeValue, TrainingExamples) ->
	AttributePos = get_attribute_pos(Attributes, AttributeName),
	filter_tes_test_acu(Attributes, AttributePos, AttributeValue, TrainingExamples, []).

filter_tes_test_acu(_Attributes, _AttributePos, _AttributeValue, [] = _TrainingExamples, Acu) ->
	lists:reverse(Acu);
filter_tes_test_acu(Attributes, AttributePos, {"<", TestValue} = AttributeValue, [CurrentExample|TrainingExamples], Acu) ->
	CurrentValue = mllib_tools:get_n_attribute_value(CurrentExample, AttributePos),
	Attribute = mllib_tools:get_n_attribute(Attributes, AttributePos),
	case mllib_tools:compare(Attribute, CurrentValue, TestValue) of
		lower           -> filter_tes_test_acu(Attributes, AttributePos, AttributeValue, TrainingExamples, [CurrentExample|Acu]);
		_GreaterOrEqual -> filter_tes_test_acu(Attributes, AttributePos, AttributeValue, TrainingExamples, Acu)
	end;
filter_tes_test_acu(Attributes, AttributePos, {">=", TestValue} = AttributeValue, [CurrentExample|TrainingExamples], Acu) ->
	CurrentValue = mllib_tools:get_n_attribute_value(CurrentExample, AttributePos),
	Attribute = mllib_tools:get_n_attribute(Attributes, AttributePos),
	case mllib_tools:compare(Attribute, CurrentValue, TestValue) of
		lower           -> filter_tes_test_acu(Attributes, AttributePos, AttributeValue, TrainingExamples, Acu);
		_GreaterOrEqual -> filter_tes_test_acu(Attributes, AttributePos, AttributeValue, TrainingExamples, [CurrentExample|Acu])
	end;
filter_tes_test_acu(Attributes, AttributePos, AttributeValue, [CurrentExample|TrainingExamples], Acu) ->
	case mllib_tools:get_n_attribute_value(CurrentExample, AttributePos) == AttributeValue of
		true ->  filter_tes_test_acu(Attributes, AttributePos, AttributeValue, TrainingExamples, [CurrentExample|Acu]);
		false -> filter_tes_test_acu(Attributes, AttributePos, AttributeValue, TrainingExamples, Acu)
	end.

%%---------------------------------------------------------------------------------------

% TODO fix doc
-spec create_training_examples_groups(Attributes :: [attribute()], AttributeName :: name() | attribute() | tuple()) -> EmptyGroups :: [{AttributePos :: integer(), AttributeValue :: value(), []}].
% @spec create_training_examples_groups(Attributes, AttributeName) -> [{AttributePos, AttributeValue, []}] where Attributes = [attribute()], AttributeName = name(), AttributePos = integer(), AttributeValue = value() 
% @doc Create list containing separated TrainingExamples that match AttributeName specific AttributeValue
create_training_examples_groups(Attributes, #attribute{} = Attribute) ->
	Values = Attribute#attribute.values,
	?DETAIL("In create_training_examples_groups/4: Attributes: ~p~nAttribute: ~p~n--~n",
		[Attributes, Attribute]),
	create_teg(get_attribute_pos(Attributes, Attribute), Values, []);
create_training_examples_groups(Attributes, {#attribute{} = Attribute, SplitValue}) ->
	AttributePos = get_attribute_pos(Attributes, Attribute),
	[{AttributePos, {"<", SplitValue}, []}, {AttributePos, {">=", SplitValue}, []}];
create_training_examples_groups(Attributes, {AttributeName, SplitValue}) ->
	create_training_examples_groups(Attributes, {get_attribute(Attributes, AttributeName), SplitValue});
create_training_examples_groups(Attributes, AttributeName) ->
	Attribute = get_attribute(Attributes, AttributeName),
	create_training_examples_groups(Attributes, Attribute).
	
%%---------------------------------------------------------------------------------------

-spec add_training_example_to_group(Group :: [{AttributePos :: integer(), Value :: value(), MatchingExamples :: [training_example()]}], TrainingExample :: training_example()) -> NewGroup :: [{AttributePos :: integer(), Value :: value(), MatchingExamples :: [training_example()]}].
% @spec add_training_example_to_group(Group, TrainingExample) -> NewGroup where Group = group(), NewGroup= group(), group() = [{AttributePos, Value, MatchingExamples}], AttributePos = integer(), Value = value(), MatchingExamples = [training_example()]
% @doc Add TrainingExample to the proper group of Examples matching the value(none if Value isn't in Group). For ordered type attributes the last argument is a pair of {Attributes, TrainingExample} or {Attribute, TrainingExample}<br/>
add_training_example_to_group([], _TrainingExample)-> 
	[];
add_training_example_to_group([{AttributePos, {"<", SplitValue}, LowerTEs}, {AttributePos, {">=", SplitValue}, GreaterTEs}], 
				{#attribute{} = Attribute, {DataAttribs, _Class} = TrainingExample})-> 
	case compare(Attribute, erlang:element(AttributePos, DataAttribs), SplitValue) of
		lower  -> NewLowerTEs = [TrainingExample|LowerTEs], NewGreaterTEs = GreaterTEs;
		equal  -> NewLowerTEs = LowerTEs, NewGreaterTEs = [TrainingExample|GreaterTEs];
		greater -> NewLowerTEs = LowerTEs, NewGreaterTEs = [TrainingExample|GreaterTEs]
	end,
	[{AttributePos, {"<", SplitValue}, NewLowerTEs}, {AttributePos, {">=", SplitValue}, NewGreaterTEs}];
add_training_example_to_group([{AttributePos, {"<", SplitValue}, LowerTEs}, {AttributePos, {">=", SplitValue}, GreaterTEs}], 
				{Attributes, {DataAttribs, _Class} = TrainingExample})-> 
	Attribute = get_n_attribute(Attributes, AttributePos),
	case compare(Attribute, erlang:element(AttributePos, DataAttribs), SplitValue) of
		lower  -> NewLowerTEs = [TrainingExample|LowerTEs], NewGreaterTEs = GreaterTEs;
		equal  -> NewLowerTEs = LowerTEs, NewGreaterTEs = [TrainingExample|GreaterTEs];
		greater -> NewLowerTEs = LowerTEs, NewGreaterTEs = [TrainingExample|GreaterTEs]
	end,
	[{AttributePos, {"<", SplitValue}, NewLowerTEs}, {AttributePos, {">=", SplitValue}, NewGreaterTEs}];
add_training_example_to_group([{AttributePos, Value, TrainingExamples} = H|Group], {DataAttribs, _Class} = TrainingExample)-> 
	case erlang:element(AttributePos, DataAttribs) of
		Value -> [{AttributePos, Value, [TrainingExample|TrainingExamples]}|Group];
		_ -> [ H | add_training_example_to_group(Group, TrainingExample)]
	end.

%%----------------------------------------------------------------------------------------

-spec fetch_training_examples_from_group(Group :: [{AttributePos :: integer(), Value :: value(), MatchingExamples :: [training_example()]}], AttributeValue :: value()) -> MatchingExamples :: [training_example()].
% @spec fetch_training_examples_from_group(Group, AttributeValue) -> MatchingExamples where Group = group(), group() = [{AttributePos, AttributeValue, MatchingExamples}], AttributePos = integer(), AttributeValue = value(), MatchingExamples = [training_example()]
% @doc Fetch TrainingExamples list matching the AttributeValue<br/>
fetch_training_examples_from_group([], _AttributeValue)-> 
	{error};
fetch_training_examples_from_group([{AttributePos, {"<", _Value} = LowerExpr, LowerTEs}, 
				{AttributePos, {">=", _Value} = _GreaterExpr, _GreaterTEs}],
				LowerExpr)-> 
	LowerTEs;
fetch_training_examples_from_group([{AttributePos, {"<", _Value} = _LowerExpr, _LowerTEs},
				{AttributePos, {">=", _Value} = GreaterExpr, GreaterTEs}],
				GreaterExpr)-> 
	GreaterTEs;
fetch_training_examples_from_group([{_AttributePos, AttributeValue, MatchingExamples} = _H|_Group], AttributeValue)-> 
	MatchingExamples;
fetch_training_examples_from_group([_Other|Group], AttributeValue)-> 
	fetch_training_examples_from_group(Group, AttributeValue).

%%---------------------------------------------------------------------------------------

% @doc It shouldn't be used to split examples to lower/greater comparable attribute's values,Groups created by mllib_tools:create_training_examples_groups/2
group_trainingexamples([], Groups) ->
	Groups;
group_trainingexamples([TrainingExample|TrainingExamples], Groups) ->
	?DETAIL("In group_trainingexamples/2: adding ~p to groups ~p~n--~n", [TrainingExample, Groups]),
	group_trainingexamples(TrainingExamples, mllib_tools:add_training_example_to_group(Groups, TrainingExample)).

%%---------------------------------------------------------------------------------------

% @doc Put examples into groups according to the test result. Groups created by mllib_tools:create_training_examples_groups/2
group_trainingexamples(TrainingExamples, Attributes, #attribute{} = Attribute) ->
	group_trainingexamples(TrainingExamples, mllib_tools:create_training_examples_groups(Attributes, Attribute));
group_trainingexamples(TrainingExamples, Attributes, AttributeTest) ->
	%% ordered or continuous
	Groups = mllib_tools:create_training_examples_groups(Attributes, AttributeTest),
	[{AttributePos, _, _}|_T] = Groups,
	group_comparable_trainingexamples(TrainingExamples, get_n_attribute(Attributes, AttributePos), Groups).

%%---------------------------------------------------------------------------------------

% @doc Put examples into groups according to the test result. Groups created by mllib_tools:create_training_examples_groups/2
group_comparable_trainingexamples([], _Attribute, Groups) ->
	?DETAIL("In group_comparable_trainingexamples/2: return~n--~n", []),
	Groups;
group_comparable_trainingexamples([TrainingExample|TrainingExamples], Attribute, Groups) ->
	?DETAIL("In group_comparable_trainingexamples/2: adding ~p to groups ~p~n--~n", [TrainingExample, Groups]),
	group_comparable_trainingexamples(TrainingExamples, Attribute, mllib_tools:add_training_example_to_group(Groups, {Attribute, TrainingExample})).


%%---------------------------------------------------------------------------------------

%% /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\

% @doc NumberedExamples = Example + its id in ETS table. Groups created by mllib_tools:create_training_examples_groups/2
group_numberedexamples(NumberedExamples, Attributes, #attribute{} = Attribute) ->
	group_numberedexamples(NumberedExamples, mllib_tools:create_training_examples_groups(Attributes, Attribute));
group_numberedexamples(NumberedExamples, Attributes, AttributeTest) ->
	%% ordered or continuous
	Groups = mllib_tools:create_training_examples_groups(Attributes, AttributeTest),
	[{AttributePos, _, _}|_T] = Groups,
	group_comparable_numberedexamples(NumberedExamples, get_n_attribute(Attributes, AttributePos), Groups).

% @doc NumberedExamples = Example + its id in ETS table. Groups created by mllib_tools:create_training_examples_groups/2
group_comparable_numberedexamples([], _Attribute, Groups) ->
	?DETAIL("In group_comparable_numberedexamples/2: return~n--~n", []),
	Groups;
group_comparable_numberedexamples([NumberedExample|NumberedExamples], Attribute, Groups) ->
	?DETAIL("In group_comparable_Numberedexamples/2: adding ~p to groups ~p~n--~n", [NumberedExample, Groups]),
	group_comparable_numberedexamples(NumberedExamples, Attribute, mllib_tools:add_numbered_example_to_group(Groups, {Attribute, NumberedExample})).

% @doc It shouldn't be used to split examples to lower/greater comparable attribute's values. Groups created by mllib_tools:create_training_examples_groups/2
group_numberedexamples([], Groups) ->
	Groups;
group_numberedexamples([NumberedExample|NumberedExamples], Groups) ->
	?DETAIL("In group_numberedexamples/2: adding ~p to groups ~p~n--~n", [NumberedExample, Groups]),
	group_numberedexamples(NumberedExamples, mllib_tools:add_numbered_example_to_group(Groups, NumberedExample)).

%%---------------------------------------------------------------------------------------

-spec add_numbered_example_to_group(Group :: [{AttributePos :: integer(), Value :: value(), MatchingExamples :: [{number(), training_example()}]}], NumberedExample :: {number(), training_example()}) -> NewGroup :: [{AttributePos :: integer(), Value :: value(), MatchingExamples :: [{number(), training_example()}]}].
% @spec add_numbered_example_to_group(Group, NumberedExample) -> NewGroup where Group = group(), NewGroup= group(), group() = [{AttributePos, Value, MatchingExamples}], AttributePos = integer(), Value = value(), MatchingExamples = [{number(), training_example()}]
% @doc Add NumberedExample to the proper group of Examples matching the value(none if Value isn't in Group). For ordered type attributes the last argument is a pair of {Attributes, NumberedExample} or {Attribute, NumberedExample}<br/>
add_numbered_example_to_group([], _NumberedExample)-> 
	[];
add_numbered_example_to_group([{AttributePos, {"<", SplitValue}, LowerTEs}, {AttributePos, {">=", SplitValue}, GreaterTEs}], 
				{#attribute{} = Attribute, {_Number, {DataAttribs, _Class}=_TrainingExample} = NumberedExample })-> 
	case compare(Attribute, erlang:element(AttributePos, DataAttribs), SplitValue) of
		lower  -> NewLowerTEs = [NumberedExample|LowerTEs], NewGreaterTEs = GreaterTEs;
		equal  -> NewLowerTEs = LowerTEs, NewGreaterTEs = [NumberedExample|GreaterTEs];
		greater -> NewLowerTEs = LowerTEs, NewGreaterTEs = [NumberedExample|GreaterTEs]
	end,
	[{AttributePos, {"<", SplitValue}, NewLowerTEs}, {AttributePos, {">=", SplitValue}, NewGreaterTEs}];
add_numbered_example_to_group([{AttributePos, {"<", SplitValue}, LowerTEs}, {AttributePos, {">=", SplitValue}, GreaterTEs}], 
				{Attributes, {_Number, {DataAttribs, _Class} = _TrainingExample}=NumberedExample})-> 
	Attribute = get_n_attribute(Attributes, AttributePos),
	case compare(Attribute, erlang:element(AttributePos, DataAttribs), SplitValue) of
		lower  -> NewLowerTEs = [NumberedExample|LowerTEs], NewGreaterTEs = GreaterTEs;
		equal  -> NewLowerTEs = LowerTEs, NewGreaterTEs = [NumberedExample|GreaterTEs];
		greater -> NewLowerTEs = LowerTEs, NewGreaterTEs = [NumberedExample|GreaterTEs]
	end,
	[{AttributePos, {"<", SplitValue}, NewLowerTEs}, {AttributePos, {">=", SplitValue}, NewGreaterTEs}];
add_numbered_example_to_group([{AttributePos, Value, NumberedExamples} = H|Group], {_Number, {DataAttribs, _Class} = _TrainingExample} = NumberedExample)-> 
	case erlang:element(AttributePos, DataAttribs) of
		Value -> [{AttributePos, Value, [NumberedExample|NumberedExamples]}|Group];
		_ -> [ H | add_numbered_example_to_group(Group, NumberedExample)]
	end.

%%---------------------------------------------------------------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%% ordering %%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc returns: {Lower, SplitValue, BiggerEq}
split_trainingexamples(TrainingExamples, Attributes, AttributeName, SplitValue) ->
	Attribute = get_attribute(Attributes, AttributeName),
	AttributePos = get_attribute_pos(Attributes, AttributeName),
	split_te_value(AttributePos, Attribute, TrainingExamples, {[], SplitValue, []}).

%%---------------------------------------------------------------------------------------
% @doc works for continuous values
compare(Value1, Value2) when is_number(Value1) and is_number(Value2) ->
	Diff = Value1 - Value2,
	if 
	  Diff < 0 ->  lower;
	  Diff == 0 -> equal;
	  Diff > 0 ->  greater
	end.

compare_te(Attributes, Attribute, TE1, TE2) when is_list(Attributes) ->
	compare_te(get_attribute_pos(Attributes, Attribute), Attribute, TE1, TE2);
compare_te(Pos, Attribute, TE1, TE2) ->
   Val1 = get_n_attribute_value(TE1, Pos), 
   Val2 = get_n_attribute_value(TE2, Pos),
   case Attribute#attribute.type of
     ordered -> compare(Attribute, Val1, Val2);
     continuous -> compare(Val1, Val2)
   end.

%%---------------------------------------------------------------------------------------
% @doc works for ordered and continuous values
compare(_Attribute, SameValue, SameValue) ->
	equal;
compare(#attribute{type = continuous} = _Attribute, Value1, Value2) ->
	compare(Value1, Value2);
compare(#attribute{type = ordered} = Attribute, Value1, Value2) ->
	case get_first_occured(Attribute#attribute.values, Value1, Value2) of
		Value1 -> lower;
		Value2 -> greater
	end.

%%---------------------------------------------------------------------------------------
% @doc works for ordered and continuous values
sort_trainingexamples(TrainingExamples, AttributePos, #attribute{} = Attribute) when is_number(AttributePos) ->
	CurrentComparer = fun(X,Y) ->
			case compare_te(AttributePos, Attribute, X, Y) of
				lower -> true;
				equal -> true;
				greater -> false
			end
		end,

	lists:sort(CurrentComparer, TrainingExamples);
sort_trainingexamples(TrainingExamples, Attributes, #attribute{} = Attribute) ->
	sort_trainingexamples(TrainingExamples, get_attribute_pos(Attributes, Attribute), Attribute);
sort_trainingexamples(TrainingExamples, Attributes, AttributeName) ->
	sort_trainingexamples(TrainingExamples, Attributes, get_attribute(Attributes, AttributeName)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Category counters  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% @doc create counters that can collect info how many examples there are in each category
create_counter_foreach_category(Categories) ->
	create_counter_foreach_category(Categories, []).	

% [{Category, Count}]
% creates counters that can be used to count how many of given training examples
% belong to a proper class' categorys
% result should be used as second argument in count_trainingexamples_categories/2
create_counter_foreach_category([], Acu) ->
	lists:reverse(Acu);
create_counter_foreach_category([Category|Categories], Acu) ->
	create_counter_foreach_category(Categories, [{Category, 0}|Acu]).
%% -----------------------------------------------------------------------------



% @doc [{Category, Count}] count how many of given training examples belong to a proper class' category, second argument should be obtained by create_counter_foreach_category/1
count_trainingexamples_categories([], CategoryCounters) ->
	CategoryCounters;
count_trainingexamples_categories([{_DataAtribs, Category} = _TrainingExamples|TrainingExamples], CategoryCounters) ->
	count_trainingexamples_categories(TrainingExamples, increase_category_class(Category, CategoryCounters)).

increase_category_class(Category, [{Category, N}|CategoryCounters]) ->
	[{Category, N+1}|CategoryCounters];
increase_category_class(Category, [_Other|CategoryCounters]) ->
	[_Other|increase_category_class(Category, CategoryCounters)].

%% -----------------------------------------------------------------------------


% @doc returns : see count_trainingexamples_categories/2
get_trainingexamples_categories_counters(Categories, TrainingExamples) ->
	count_trainingexamples_categories(TrainingExamples, create_counter_foreach_category(Categories, [])).

%%%%%%%
%%%%%%% Structures API
%%%%%%%

-spec get_attribute(Attributes :: [attribute()], AttributeName :: name()) -> Attribute :: attribute().
% @spec get_attribute(Attributes, AttributeName) -> Attribute where Attributes = [attribute()], AttributeName = name(), Attribute = attribute()
% @doc Get the given AttributeName attribute record from Attributes.
get_attribute([], _AttributeName) ->
	throw({error, no_such_attribute});
get_attribute([ #attribute{ name = AttributeName} = Searched | _Attributes], AttributeName) ->
	Searched;
get_attribute([ _Other | T], AttributeName) ->
	get_attribute(T, AttributeName).
%%---------------------------------------------------------------------------------------

-spec get_n_attribute(Attributes :: [attribute()], AttributePos :: name()) -> Attribute :: attribute().
% @spec get_n_attribute(Attributes, AttributePos) -> Attribute where Attributes = [attribute()], AttributePos = number(), Attribute = attribute()
% @doc Get the given AttributePos attribute record from Attributes.
get_n_attribute(Attributes, AttributePos) ->
	lists:nth(AttributePos, Attributes).
%%---------------------------------------------------------------------------------------

-spec exclude(List :: [any()], Number :: integer()) -> ListWithoutNthElement :: [any()].
% @spec exclude(List, Number) -> ListWithoutNthElement where List = [any()], Number = integer(), ListWithoutNthElement = [any()]
% @doc Remove nth argument from the list.
exclude(List, Number) ->
	exclude_no(List, Number, 1, []).

exclude_no([ToReturn | List], Number, Number, Acu) ->
	{ lists:reverse(Acu) ++ List, ToReturn };
exclude_no([Other | List], Number, Act, Acu) ->
	exclude_no(List, Number, Act + 1, [Other|Acu]).

-spec exclude_multi(List :: [any()], Numbers :: [integer()]) -> ListWithoutNthElements :: [any()].
% @spec exclude_multi(List, Numbers) -> ListWithoutNthElements where List = [any()], Numbers = [integer()], ListWithoutNthElements = [any()]
% @doc Remove elements whose positions are specified in the Numbers list from the input List.
exclude_multi(List, Numbers) ->
	exclude_multi(List, lists:sort(Numbers), 1, []).

exclude_multi([_|T1], [CurrentPos|T2], CurrentPos, Acc) ->
	exclude_multi(T1, T2, CurrentPos + 1, Acc);
exclude_multi([Elem|T1], [ExcludePos|T2], CurrentPos, Acc) ->
	exclude_multi(T1, [ExcludePos|T2], CurrentPos + 1, [Elem|Acc]);
exclude_multi([], _, _, Acc) ->
	lists:reverse(Acc);
exclude_multi(Rest, [], _, Acc) ->
	lists:reverse(Acc) ++ Rest.

%% -----------------------------------------------------------------------------
-spec denumber_tes(NumberedExamples :: [{number(), training_example()}]) -> TrainingExamples :: [training_example()].
% @spec denumber_tes(NumberedExamples) -> TrainingExamples where NumberedExamples = [{number(), training_example()}], TrainingExamples = [training_example()]
% @doc Remove a number attached to a TrainingExample<br/>
denumber_tes(NumberedExamples) ->
	denumber_tes(NumberedExamples, []).

denumber_tes([], Acu) ->
	lists:reverse(Acu);
denumber_tes([{_N, Data}|NumberedExamples], Acu) ->
	denumber_tes(NumberedExamples, [Data|Acu]).

%%---------------------------------------------------------------------------------------

-spec get_tes(Ets :: atom(), Numbers :: [number()]) -> NumberedExamples :: [{number(), training_example()}].
% @spec get_tes(Ets, Numbers) -> NumberedExamples where Ets = atom(), Numbers = [number()], NumberedExamples = [{number(), training_example()}]
% @doc Get TrainingExamples data from ets table identified by Ets. Returns a list of numbered TrainingExamples, that is pairs of {Number, TrainingExample}.
get_tes(Ets, Numbers) ->
	get_tes_acu(Ets, Numbers, [], []).

get_tes_acu(_Ets, [], NumberAcu, TEAcu) ->
	{lists:reverse(NumberAcu), lists:reverse(TEAcu)};
get_tes_acu(Ets, [Number|Numbers], NumberAcu, TEAcu) ->
	[{Number, TE}] = ets:lookup(Ets, Number),
	get_tes_acu(Ets, Numbers, [{Number, TE}|NumberAcu], [TE|TEAcu]).

%%---------------------------------------------------------------------------------------

-spec get_tes_data(Ets :: atom(), Numbers :: [number()]) -> TrainingExamples :: [training_example()].
% @spec get_tes_data(Ets, Numbers) -> TrainingExamples where Ets = atom(), Numbers = [number()], TrainingExamples = [training_example()]
% @doc Get TrainingExamples data from ets table identified by Ets. Returns a list of TrainingExamples.
get_tes_data(Ets, Numbers) ->
	get_tes_data_acu(Ets, Numbers, []).

get_tes_data_acu(_Ets, [], Acu) ->
	lists:reverse(Acu);
get_tes_data_acu(Ets, [Number|Numbers], Acu) ->
	[{Number, TE}] = ets:lookup(Ets, Number),
	get_tes_data_acu(Ets, Numbers, [TE|Acu]).

%%---------------------------------------------------------------------------------------
% @doc get all training examples from ETS 
get_all_tes(Ets) ->
	denumber_tes(ets:select(Ets,[{'$1',[],['$_']}])).

%%---------------------------------------------------------------------------------------

-spec get_numbers(NumberedExamples :: [{number(), training_example()}]) -> Numbers :: [number()].
% @spec get_numbers(NumberedExamples) -> Numbers where NumberedExamples = [{number(), training_example()}], Numbers = [number()]
% @doc Get Numbers of NumberedExamples.
get_numbers(NumberedExamples) ->
	get_numbers_acu(NumberedExamples, []).

get_numbers_acu([] = _NumberedExamples, Acu) ->
	lists:reverse(Acu);
get_numbers_acu([{Number, _TE} | NumberedExamples], Acu) ->
	get_numbers_acu(NumberedExamples, [Number|Acu]).

%%----------------------------------------------------------------------------------------
% @doc returns {CorrectlyClassified, IncorrectlyClassified, Errors}
compute_error(Classifier, Examples, Options) ->
	perform_classify_test(Classifier, Examples, Options, 0, 0, 0).

perform_classify_test(_Classifier, [] = _Examples, _Options, Correct, Incorrect, Errors) ->
	{Correct, Incorrect, Errors};
perform_classify_test(Classifier, [{Example, ActualCategory}|T], Options, Correct, Incorrect, Errors) ->
	case mllib:classify(Classifier, Example, Options) of
		{ok, ActualCategory} -> perform_classify_test(Classifier, T, Options, Correct + 1, Incorrect, Errors);
		{ok, _} -> perform_classify_test(Classifier, T, Options, Correct, Incorrect + 1, Errors);
		{error, _} -> perform_classify_test(Classifier, T, Options, Correct, Incorrect, Errors + 1)
	end.


%%
%% Local Functions
%%

get_attribute_pos([], _AttributeName, _N) ->
	error;
get_attribute_pos([ #attribute{name = AttributeName} | _T ], AttributeName, Pos ) ->
	?DETAIL("In get_attribute_pos/3: match : AttributeName: ~p~nPos: ~p~n--~n",
		[AttributeName, Pos]),
	Pos;
get_attribute_pos([_Other|Tail], AttributeName, Pos) ->
	?DETAIL("In get_attribute_pos/3: other : AttributeName: ~p~nSearching: ~p~nPos: ~p~n--~n",
		[Other#attribute.name, AttributeName, Pos]),
	get_attribute_pos(Tail, AttributeName, Pos + 1).

get_first_occured([Value1|_Values], Value1, _Value2) ->
	Value1;
get_first_occured([Value2|_Values], _Value1, Value2) ->
	Value2;
get_first_occured([_Other|Values], Value1, Value2) ->
	get_first_occured(Values, Value1, Value2).

get_pos([Value|_Values], Value, N) ->
	N;
get_pos([_Other|Values], Value, N) ->
	get_pos(Values, Value, N+1).


create_teg(_AttributePos, [], Acu) ->
	lists:reverse(Acu);
create_teg(AttributePos, [Value|T], Acu) ->
	create_teg(AttributePos, T, [{AttributePos, Value,[]} | Acu]).

split_te_value(_AttributePos, _Attribute, [], {Lower, SplitValue, BiggerEq}) ->
	{lists:reverse(Lower), SplitValue, lists:reverse(BiggerEq)};
split_te_value(AttributePos, Attribute, [TrainingExample | TrainingExamples], {Lower, SplitValue, BiggerEq}) ->
	Value = get_n_attribute_value(TrainingExample, AttributePos),
	Comparision = case Attribute#attribute.type of
		ordered -> compare(Attribute, Value, SplitValue);
		continuous -> compare(Value, SplitValue)
		end,
	case Comparision of 
		lower   -> split_te_value(AttributePos, Attribute, TrainingExamples, {[TrainingExample | Lower], SplitValue, BiggerEq});
		equal   -> split_te_value(AttributePos, Attribute, TrainingExamples, {Lower, SplitValue, [TrainingExample | BiggerEq]});
		greater -> split_te_value(AttributePos, Attribute, TrainingExamples, {Lower, SplitValue, [TrainingExample | BiggerEq]})
	end.


replace_nth([_OldValue | List], N, NewValue, I) when N == I ->
	[NewValue | List];
replace_nth([Other | List], N, NewValue, I) ->
	[Other | replace_nth(List, N, NewValue, I+1)].



