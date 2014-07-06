-module(transform).

%%
%% Include files
%%

-include("structures.hrl").
-include("debug.hrl").


%%
%% Exported Functions
%%

-export([transform_attributes/5, transform_example/2]).

%%
%% API Functions
%%

-spec transform_attributes(Type :: atom(), Attributes :: [attribute()], Class :: class(), TrainingExamples :: [training_example()], Options :: [Option :: any()]) -> {NewAttributes :: [attribute()], Transformer :: transformer()} | {error, Reason :: any()}.
% @spec transform_attributes(Type, Attributes, Class, TrainingExamples, Options) -> {NewAttributes, Transformer} | {error, Reason} where Type = atom(), Attributes = [attribute()], Class = class(), TrainingExamples = [training_example()], Options = [any()], NewAttributes = [attribute()], Transformer = transformer, Reason = any()
% @doc Returns new definition of attributes NewAttributes and Transformer- the structure that is used to transform (training) examples. The possible Type values are : * discrete.
transform_attributes(discrete = _Type, Attributes, Class, TrainingExamples, Options) ->
	discrete_attrib(Attributes, Class, TrainingExamples, Options).

-spec transform_example(Transformer :: transformer(), Example :: training_example() | example()) -> TransformedExample :: training_example() | example() | {error, Reason :: any()}.
% @spec transform_example(Transformer, Example) -> TransformedExample | {error, Reason} where Transformer = transformer(), Example = training_example() | example(), TransformedExample = training_example() | example(), Reason = any()
% @doc Returns the example(or Training Example) transformed into new definition.
transform_example(Transformer, {Example, Category} = _TrainingExample) when is_tuple(Example) ->
	{transform_example(Transformer, Example), Category};
transform_example(#transformer{t_fun = TransFun} = Transformer, Example) ->
	TransFun(Transformer, Example).
	

%%
%% Local Functions
%%	

%%    --*-*---*-*-*-**-*-*-*-*-*-*-*----***-*-*--**--*-*
swap_attribute([Attribute|Attributes], Attribute, NewAttribute) ->
	[NewAttribute | Attributes];
swap_attribute([OtherAttribute|Attributes], Attribute, NewAttribute) ->
	[OtherAttribute | swap_attribute(Attributes, Attribute, NewAttribute)].

%%    --*-*---*-*-*-**-*-*-*-*-*-*-*----***-*-*--**--*-*

discrete_attrib(Attributes, Class, TrainingExamples, Options) ->
	{name, AttributeName} = lists:keyfind(name, 1, Options),
	Method = case lists:keyfind(method, 1, Options) of
		false -> information;
		{method, PassedMethod} -> PassedMethod
		end,

	Attribute = mllib_tools:get_attribute(Attributes, AttributeName),
	Splitters = case Method of
			information -> information_discrete(Attributes, Class, TrainingExamples, Attribute);
			{in_interval, N} -> discrete_n_in_intervals(Attributes, TrainingExamples, Attribute, N);
			{intervals, N} -> discrete_intevals(Attribute, N)
			end,

	{_LeftBound, RightBound} = Attribute#attribute.values,

	Boundaries = Splitters ++ [RightBound],
	Values = lists:seq(1, length(Boundaries)),
	

	AttributeName = Attribute#attribute.name,
	NewAttributes = swap_attribute(Attributes, Attribute, #attribute{name=AttributeName, type=ordered, values=Values}),
	Transformer = #transformer{type = discrete, data = {Attributes, NewAttributes, AttributeName, Boundaries}, t_fun = fun te_discrete/2}, 
	{NewAttributes, Transformer}.


%%    --*-*---*-*-*-**-*-*-*-*-*-*-*----***-*-*--**--*-*
arrange_bounds([] = _Splitters, LeftBound, RightBound, Result) ->
	Result1 = [{LeftBound, RightBound}|Result],
	lists:reverse(Result1);
arrange_bounds([Splitter|Splitters], LeftBound, RightBound, Result) when Splitter > LeftBound, Splitter < RightBound ->
	arrange_bounds(Splitters, Splitter, RightBound, [{LeftBound, Splitter}|Result]).

%%    --*-*---*-*-*-**-*-*-*-*-*-*-*----***-*-*--**--*-*
information_discrete(Attributes, Class, TrainingExamples, Attribute) ->
	Entropies = information:get_entropies(Attributes, Class, TrainingExamples, [Attribute]),
	?LOG("Entropies: ~p..~n", [Entropies]),
	{MinValue, MinEntropy} = get_min_entropy(Entropies, {none, 2}),
	InformationGrowth = information:get_information(Class, TrainingExamples) - MinEntropy,
	if 
	  InformationGrowth =< 0 -> ?LOG("~p..~n", [InformationGrowth]), [];
	  true -> 
		?LOG("~p..~n", [InformationGrowth]),
		{Lower, MinValue, Bigger} = 
		mllib_tools:split_trainingexamples(TrainingExamples, Attributes, Attribute#attribute.name, MinValue),
		
		information_discrete(Attributes, Class, Lower, Attribute) 
		++ [MinValue] 
		++ information_discrete(Attributes, Class, Bigger, Attribute)
	end.

%% ===
discrete_n_in_intervals(Attributes, TrainingExamples, Attribute, N) ->
	AttributePos = mllib_tools:get_attribute_pos(Attributes, Attribute),
	SortedTEs = mllib_tools:sort_trainingexamples(TrainingExamples, Attributes, Attribute),
	[FirstValue|Sorted] = SortedTEs,	
	discrete_n_in_intervals(Sorted, FirstValue, AttributePos, N, 2, []).

discrete_n_in_intervals([], _LastValue, _Pos, _N, _SomeN, Bounds) ->
	lists:reverse(Bounds);
discrete_n_in_intervals([TE | TrainingExamples], LastValue, Pos, N, CurrentN, Bounds) when CurrentN == N + 1 ->
	NewBound = mllib_tools:get_n_attribute_value(TE, Pos),
	if LastValue == NewBound -> discrete_n_in_intervals(TrainingExamples, LastValue, Pos, N, N+1, Bounds);
  	                    true -> discrete_n_in_intervals(TrainingExamples, NewBound, Pos, N, 2, [NewBound | Bounds])
	end;
discrete_n_in_intervals([TE | TrainingExamples], _LastValue, Pos, N, CurrentN, Bounds) ->
	discrete_n_in_intervals(TrainingExamples, mllib_tools:get_n_attribute_value(TE, Pos), Pos, N, CurrentN + 1, Bounds).
	
%% ===
discrete_intevals(#attribute{values = {LeftBound, RightBound}} = _Attribute, N) ->
	Step = (RightBound - LeftBound) / N,
	discrete_intevals(LeftBound, Step, N, 1, []).

discrete_intevals(_LeftBound, _Step, N, N, Bounds) ->
	lists:reverse(Bounds);
discrete_intevals(LeftBound, Step, N, CurrentN, Bounds) ->
	discrete_intevals(LeftBound, Step, N, CurrentN + 1, [ (LeftBound + Step*CurrentN) | Bounds]).

%%    --*-*---*-*-*-**-*-*-*-*-*-*-*----***-*-*--**--*-*
get_min_entropy([], Min) ->
	Min;
get_min_entropy([{{_AttributeName, Value}, continuous, Entropy}|Entropies], {_MinValue, MinEntropy}) when Entropy < MinEntropy ->
	get_min_entropy(Entropies, {Value,Entropy});
get_min_entropy([_Bigger|Entropies], Min) ->
	get_min_entropy(Entropies, Min).

%%    --*-*---*-*-*-**-*-*-*-*-*-*-*----***-*-*--**--*-*
te_discrete(#transformer{ data={OldAttributes, NewAttributes, AttributeName, Bounds}}, Example) ->
	OldValue = mllib_tools:get_attribute_value(OldAttributes, Example, AttributeName),
	WhichBound = match_the_bound(Bounds , OldValue, 1),
	NewValue = mllib_tools:get_attribute_nth_value(NewAttributes, AttributeName, WhichBound),
	mllib_tools:set_attribute_value(NewAttributes, Example, AttributeName, NewValue).

%%    --*-*---*-*-*-**-*-*-*-*-*-*-*----***-*-*--**--*-*
match_the_bound([Right|_Bounds], Value, N) when Value < Right ->
	N;
match_the_bound([_Other|Bounds], Value, N) ->
	match_the_bound(Bounds, Value, N+1).


