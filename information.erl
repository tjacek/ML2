%%%
%%% Information theory algorithms
%%%

-module(information).

%%
%% Include files
%%

-include("structures.hrl").
-include("debug.hrl").

%%
%% Exported Functions
%%

-export([get_entropies/4]).
-export([get_information/2, get_information_growths/4, get_information_growth_factors/4]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%    entropy   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
%%% DOCU TODO!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! %%%%% /\/\/\/\/\/\
%% /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
get_entropies(Attributes, Class, TrainingExamples, AttributesToCount) ->
	get_entropies_acu(Attributes, Class, TrainingExamples, AttributesToCount, []).

get_entropy(Attributes, Class, TrainingExamples, Attribute) ->
	InitialCategoryCounters = mllib_tools:create_counter_foreach_category(Class#class.categories),
	?DETAIL("In get_entropy/4: InitialCategoryCounters: ~p~n---~n", [InitialCategoryCounters]),
	case Attribute#attribute.type of
		nominal -> Groups = mllib_tools:create_training_examples_groups(Attributes, Attribute#attribute.name),
			?DETAIL("In get_entropy/4: InitialGroups: ~p~n---~n", [Groups]),
			{Attribute#attribute.name, nominal, attribute_entropy(Attribute, TrainingExamples, 
			InitialCategoryCounters, Groups)};
		ordered ->
			SortedTrainingExamples = mllib_tools:sort_trainingexamples(TrainingExamples, Attributes, Attribute),

			[ {{Attribute#attribute.name, Value}, ordered, 
			weighted_continuous_entropy(Attributes, TrainingExamples, InitialCategoryCounters, Attribute, Value)}
			|| Value <- gen_splitters(SortedTrainingExamples, Attributes, Attribute)];
		continuous ->
			SortedTrainingExamples = mllib_tools:sort_trainingexamples(TrainingExamples, Attributes, Attribute),

			[ {{Attribute#attribute.name, Value}, continuous, 
			weighted_continuous_entropy(Attributes, TrainingExamples, InitialCategoryCounters, Attribute, Value)}
			|| Value <- gen_splitters(SortedTrainingExamples, Attributes, Attribute)]

	end.

%ordered ->
%			SortedTrainingExamples = mllib_tools:sort_trainingexamples(TrainingExamples, Attributes, Attribute),
%			Splitters = gen_splitters(SortedTrainingExamples, Attributes, Attribute),
%			GroupsList = [mllib_tools:create_training_examples_groups(Attributes, {Attribute, SplitValue}) 
%					|| SplitValue <- Splitters ],
%			
%			[{{Attribute#attribute.name, Groups}, ordered, 
%			attribute_entropy(Attribute, TrainingExamples, InitialCategoryCounters, Groups)} 
%			|| Groups <- GroupsList];

%% /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
%%% !!!!!!!!!!!!!!!!OUT OF DATE!!!!!!!!!!!!
%% /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
get_continuous_entropies(Attributes, Class, TrainingExamples, Attribute) ->
	AttributePos = mllib_tools:get_attribute_pos(Attributes, Attribute#attribute.name),
	Comparator = fun(A, B) -> mllib_tools:get_n_attribute_value(A, AttributePos) 
					=< mllib_tools:get_n_attribute_value(B, AttributePos) end,
	SortedTrainingExamples = lists:sort(Comparator, TrainingExamples),
	
	[ get_continuous_entropy(Attributes, Class, SortedTrainingExamples, Attribute, Value)
	|| Value <- generate_subvalues(SortedTrainingExamples, AttributePos)].


get_continuous_entropy(Attributes, Class, TrainingExamples, Attribute, Value) ->
	InitialCategoryCounters = mllib_tools:create_counter_foreach_category(Class#class.categories),
	{{Attribute#attribute.name, Value}, continuous, 
		weighted_continuous_entropy(Attributes, TrainingExamples, InitialCategoryCounters, Attribute, Value)}.
	

generate_subvalues([] = _TrainingExamples, _AttributePos) ->
	[];
generate_subvalues([TE|TrainingExamples], AttributePos) ->
	gen_subvals(TrainingExamples, AttributePos, 
		mllib_tools:get_n_attribute_value(TE, AttributePos), []).

gen_subvals([] = _TrainingExamples, _AttributePos, _LeftValue, Result) ->
	lists:reverse(Result);
gen_subvals([TE|TrainingExamples], AttributePos, LeftValue, Result) ->
	RightValue = mllib_tools:get_n_attribute_value(TE, AttributePos),
	?DETAIL("LeftValue : ~p , RightValue : ~p~n", [LeftValue, RightValue]),
	gen_subvals(TrainingExamples, AttributePos, 
		RightValue, [ (LeftValue + RightValue) / 2  | Result ]).

%% /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%    information    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% /\/\/\/\/\/\/\/\/\
%% /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
%%% DOCU TODO!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! %%%%% /\/\/\/\/\/\
%% /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
get_information(Class, TrainingExamples) ->
	CategoryCounters = mllib_tools:get_trainingexamples_categories_counters(Class#class.categories, TrainingExamples),
	apply_final_information_formula(length(TrainingExamples), CategoryCounters, 0).

%% /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
%%% DOCU TODO!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! %%%%% /\/\/\/\/\/\
%% /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
%% [{AttributeName, InformationGrowth}]
get_information_growths(Attributes, Class, TrainingExamples, AttributesToCount) ->
	Information = get_information(Class, TrainingExamples),
	Entropies = get_entropies(Attributes, Class, TrainingExamples, AttributesToCount),
	[ {AttributeTest, Information - Entropy} || {AttributeTest, _Type, Entropy} <- Entropies].

%% /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
%%% DOCU TODO!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! %%%%% /\/\/\/\/\/\
%% /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
%% [{AttributeTest, InformationGrowthFactor}]
get_information_growth_factors(_Attributes, _Class, [] = _TrainingExamples, _AttributesToCount) ->
	[];
get_information_growth_factors(_Attributes, _Class, _TrainingExamples, [] = _AttributesToCount) ->
	[];
get_information_growth_factors(Attributes, Class, TrainingExamples, AttributesToCount) ->
	information_growth_factors(Attributes, Class, TrainingExamples, AttributesToCount, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% helper information %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%  = sum(  - Div * log(Div)) where Div = TrainingExamplesCountedInCategory/TrainingExamplesLength
apply_final_information_formula(0 = _TrainingExamplesLength, _CategoryCounters, _Result ) ->
	0;
apply_final_information_formula( _TrainingExamplesLength, [] = _CategoryCounters, Result ) ->
	Result;
apply_final_information_formula( TrainingExamplesLength, [{_Category, CountedInCategory} | CategoryCounters], Result ) ->
	Modifier = case CountedInCategory of
			0 -> 0;
			_NonZero -> Div = CountedInCategory/TrainingExamplesLength , -Div*math:log(Div)
			end,	
	apply_final_information_formula( TrainingExamplesLength, CategoryCounters, Result + Modifier).

%%%%%%%%%%%%%
information_growth_factors(_Attributes, _Class, _TrainingExamples, [] = _AttributesToCount, Result) ->
	?DETAIL("In information_growth_factors/5: End~n=====~n", []),
	Result;
information_growth_factors(Attributes, Class, TrainingExamples, [Attribute | AttributesToCount], Result) ->
	InformationGrowths = get_information_growths(Attributes, Class, TrainingExamples, [Attribute]),
	?DETAIL("In information_growth_factors/5: InformationGrowths: ~p~n=====~n", [InformationGrowths]),
	InformationValues = [ get_information_value(Attributes, TrainingExamples, AttributeTest)
				|| {AttributeTest, _InformationGrowth} <- InformationGrowths ],
	
	
	?DETAIL("Calling collect_information_growth_factors/3: AttributeTests: ~p, InformationValues: ~p~n====~n", [InformationGrowths, InformationValues]),
	IGFS = collect_information_growth_factors(InformationGrowths, InformationValues, []),
	information_growth_factors(Attributes, Class, TrainingExamples, AttributesToCount, IGFS ++ Result).

%	[ { _AttributeName, InformationGrowth } ] = get_information_growths(Attributes, Class, TrainingExamples, [Attribute]),
%	InformationValue = get_information_value(Attributes, TrainingExamples, Attribute),
%	IGF = InformationGrowth / InformationValue,
%	information_growth_factors(Attributes, Class, TrainingExamples, AttributesToCount, 
%						[{Attribute#attribute.name, IGF} | Result]).

collect_information_growth_factors([], [], Acu) ->
	Acu;
collect_information_growth_factors([{AttributeTest, InformationGrowth}|AttributeTests], [InformationValue|InformationValues], Acu) 
	when ((InformationGrowth == 0) or (InformationValue == 0)) ->

	?DETAIL("Calling collect_information_growth_factors/3: AttributeTests: ~p, InformationValues: ~p~n====~n", [AttributeTests, InformationValues]),
	collect_information_growth_factors(AttributeTests, InformationValues, [{AttributeTest, 0} | Acu] );
collect_information_growth_factors([{AttributeTest, InformationGrowth}|AttributeTests], [InformationValue|InformationValues], Acu) ->
	?DETAIL("Calling collect_information_growth_factors/3: AttributeTests: ~p, InformationValues: ~p~n====~n", [AttributeTests, InformationValues]),
	collect_information_growth_factors(AttributeTests, InformationValues, [{AttributeTest, InformationGrowth / InformationValue} | Acu] ).


%% See Cichosz, 3.2.3 Wspolczynnik przyrostu informacji - IV_t(P)
get_information_value(Attributes, TrainingExamples, {_AttributeName, Value} = AttributeTest) -> 
	% separate training_examples and count each group's length
	?DETAIL("In get_information_value/3 : AttributeTest: ~p~n====~n", [AttributeTest]),
	Groups = mllib_tools:group_trainingexamples(TrainingExamples, Attributes, AttributeTest),
	?DETAIL("In information_growth_factors/5: Groups: ~p~n=====~n", [Groups]),
	ValuesTrainingExamplesLength = [ length( mllib_tools:fetch_training_examples_from_group(Groups, {"<", Value}) ), 
  					 length( mllib_tools:fetch_training_examples_from_group(Groups, {">=", Value}) ) ],
	TrainingExamplesLength = length(TrainingExamples),
	?DETAIL("Calling apply_final_IV_formula/3: ValuesTrainingExamplesLength : ~p, TrainingExamplesLength: ~p~n====~n", 
	[ValuesTrainingExamplesLength, TrainingExamplesLength]),
	
	apply_final_IV_formula( ValuesTrainingExamplesLength, TrainingExamplesLength, 0 );
get_information_value(Attributes, TrainingExamples, AttributeName) -> 
	% separate training_examples and count each group's length
	Attribute = mllib_tools:get_attribute(Attributes, AttributeName),
	Groups = mllib_tools:group_trainingexamples(TrainingExamples, Attributes, Attribute),
	ValuesTrainingExamplesLength = [ length( mllib_tools:fetch_training_examples_from_group(Groups, Value) )
					|| Value <- Attribute#attribute.values ],

	TrainingExamplesLength = length(TrainingExamples),
	
	apply_final_IV_formula( ValuesTrainingExamplesLength, TrainingExamplesLength, 0 ).




%% = sum - (Div * log(Div)) where Div = TrainingExamplesOfAttributeValueR / TrainingExamplesLength 
apply_final_IV_formula( [] = _ValuesTrainingExamplesLength, _TrainingExamplesLength, Result ) ->
	Result;
apply_final_IV_formula( [ VTEL | ValuesTrainingExamplesLength], TrainingExamplesLength, Result ) ->
	Modifier = case VTEL of
			0 -> 0; 
			_NonZero -> Div = VTEL / TrainingExamplesLength , -Div*math:log(Div)
			end,
	apply_final_IV_formula( ValuesTrainingExamplesLength, TrainingExamplesLength, Result + Modifier ).


	
	
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% helper entropy %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
gen_splitters(TrainingExamples, Attributes, Attribute) ->
	case Attribute#attribute.type of
		ordered -> [FirstValue|_Tail] = Attribute#attribute.values;
		continuous -> {FirstValue, _RightBound} = Attribute#attribute.values
	end,
	splitter(TrainingExamples, mllib_tools:get_attribute_pos(Attributes, Attribute),  Attribute, FirstValue, []).

splitter([], _AttributePos, _Attribute, LastValue, []) ->
	[LastValue];	
splitter([], _AttributePos, _Attribute, _LastValue, Acu) ->
	lists:reverse(Acu);
splitter([TrainingExample|TrainingExamples], AttributePos, Attribute, LastValue, Acu) ->
	CurrentValue = mllib_tools:get_n_attribute_value(TrainingExample, AttributePos),
	if CurrentValue == LastValue -> splitter(TrainingExamples, AttributePos, Attribute, LastValue, Acu);
	true -> 
		NewSplitter = get_mid_value(Attribute, LastValue, CurrentValue),
		splitter(TrainingExamples, AttributePos, Attribute, CurrentValue, [NewSplitter|Acu])
	end.

get_mid_value(#attribute{type = ordered} = Attribute, LastValue, CurrentValue) ->
	Pos1 = mllib_tools:get_attribute_value_pos(Attribute, LastValue),
	Pos2 = mllib_tools:get_attribute_value_pos(Attribute, CurrentValue),
	SplitPos = ceiling((Pos1 + Pos2)/2),
	mllib_tools:get_attribute_nth_value(Attribute, SplitPos);
get_mid_value(#attribute{type = continuous} = _Attribute, LastValue, CurrentValue) ->
	(LastValue + CurrentValue)/2.

	

attribute_entropy(Attribute, TrainingExamples, InitialCategoryCounters, InitialGroups) ->
	?DETAIL("In attribute_entropy/4: InitialGroups ~p~n--~n", [InitialGroups]),
	Groups = mllib_tools:group_trainingexamples(TrainingExamples, InitialGroups), % groups training examples to the list of TrainingExamples matching proper AttributeValue
	
	AttributesValuesCounters = [attribute_value_counter(AttributeValue, Groups, InitialCategoryCounters)
	|| AttributeValue <- Attribute#attribute.values, true],

	AttributesValuesEntropies = [ {OverallLength, attribute_value_entropy( AttributeValueCounter, 0)}
	|| {OverallLength, _Counters} = AttributeValueCounter <- AttributesValuesCounters, true],

	apply_final_entropy_formula( length(TrainingExamples), AttributesValuesEntropies, 0).

attribute_value_counter(AttributeValue, Groups, InitialCategoryCounters) ->
	TrainingExamples = mllib_tools:fetch_training_examples_from_group(Groups, AttributeValue),
	{length(TrainingExamples), mllib_tools:count_trainingexamples_categories(TrainingExamples, InitialCategoryCounters)}.


attribute_value_entropy({_OverallLength, []}, Result) ->
	Result;
attribute_value_entropy({OverallLength, [{_Category, Count}|Counters]}, Result) ->
	Modifier = case {OverallLength, Count} of
			{0, _} -> 0;
			{_, 0} -> 0;
			{OverallLength, Count} -> Div = Count/OverallLength, Div*math:log(Div)
			end,
	attribute_value_entropy({OverallLength, Counters}, Result - Modifier).

apply_final_entropy_formula( _TrainingExamplesLength, [], Result ) ->
	Result;
apply_final_entropy_formula( TrainingExamplesLength, [{AttributeValueExamplesLength, AttributeValueEntropy} | AttributesValuesEntropies], Result ) ->
	Modifier = ( AttributeValueEntropy * AttributeValueExamplesLength ) / TrainingExamplesLength,
	apply_final_entropy_formula( TrainingExamplesLength, AttributesValuesEntropies, Result + Modifier).



weighted_continuous_entropy(_Attributes, [] = _TrainingExamples, _InitialCategoryCounters, _Attribute, _Value) ->
	0;
weighted_continuous_entropy(Attributes, TrainingExamples, InitialCategoryCounters, Attribute, Value) ->
	{Lower, Value, Bigger} = mllib_tools:split_trainingexamples(TrainingExamples, Attributes, Attribute#attribute.name, Value),
	TrainingExamplesLength = length(TrainingExamples),
	
	AttributesValuesCounters = [ {length(Lower), mllib_tools:count_trainingexamples_categories(Lower, InitialCategoryCounters)},
				{length(Bigger), mllib_tools:count_trainingexamples_categories(Bigger, InitialCategoryCounters)} ],
	
	AttributesValuesEntropies = [ {OverallLength, attribute_value_entropy( AttributeValueCounter, 0)}
	|| {OverallLength, _Counters} = AttributeValueCounter <- AttributesValuesCounters, true],

	apply_final_entropy_formula( TrainingExamplesLength, AttributesValuesEntropies, 0).


ceiling(X) when X < 0 ->
    trunc(X);
ceiling(X) ->
    T = trunc(X),
    case X - T == 0 of
        true -> T;
        false -> T + 1
    end.
	


get_entropies_acu(_Attributes, _Class, _TrainingExamples, [], Acu) ->
	lists:reverse(Acu);
get_entropies_acu(Attributes, Class, TrainingExamples, [Attribute|AttributesToCount], Acu) ->
	Entropy = get_entropy(Attributes, Class, TrainingExamples, Attribute),
	if is_list(Entropy) ->	EntropiesAcu = Entropy ++ Acu;
			true -> EntropiesAcu = [Entropy | Acu]
	end,
	get_entropies_acu(Attributes, Class, TrainingExamples, AttributesToCount, EntropiesAcu).



