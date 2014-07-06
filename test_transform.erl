-module(test_transform).

%%
%% Include files
%%
-include("structures.hrl").
-include("debug.hrl").

%%
%% Exported Functions
%%
-export([test/1, test_c45/1, sort/0]).

%%
%% API Functions
%%


atts() ->
	[#attribute{name = nom, type = nominal, values = [a,b,c]},
	#attribute{name = temp, type = continuous, values = {-100, 100}},
	#attribute{name = ord, type = ordered, values = [a1, a2, a3]}].

class() ->
	#class{name = three, categories = [x, y, z]}.

te() ->
	[{{a, -2, a3}, x},
	{{a, -11, a2}, x},
	{{b, -12, a1}, x},
	{{a, 5, a2}, y},
	{{a, 8, a2}, y},
	{{b, 12, a1}, y},
	{{b, 17, a2}, z},
	{{a, 22, a1}, z},
	{{c, -11, a2}, x},
	{{a, 20, a1}, y},
	{{a, 22, a2}, z},
	{{c, 5, a3}, y},
	{{c, 1, a2}, y}].

test(Options) ->
	Attributes = atts(),
	Class = class(),
	TrainingExamples = te(),

	io:format("====================
Class: ~p~n----------
TrainingExamples: ~p~n---------
Attributes: ~p
>>>>>>>~nDiscretization: temp~n",
	[Class, TrainingExamples, Attributes]),

	{NewAttributes, Transformer} = mllib:transform_attributes(discrete, Attributes, Class, TrainingExamples, [{name, temp}] ++ Options),
	NewTE = [ mllib:transform_example(Transformer, TE) || TE <- TrainingExamples ],

	io:format("-*-*--*~nTransformer: ~p~n-*-*--*~n", [Transformer]),

	io:format("=== After ===
TrainingExamples: ~p~n-------
Attributes: ~p~n--------~n", [NewTE, NewAttributes]).




sort() ->
	Attributes = atts(),
	TrainingExamples = te(),

	io:format("==== BEFORE =======
TrainingExamples: ~p~n---------
Attributes: ~p
>>>>>>>~nSorting by: temp~n",
	[TrainingExamples, Attributes]),

	SortedTE = mllib_tools:sort_trainingexamples(TrainingExamples, Attributes, temp),

	io:format("==== SORTED =======
TrainingExamples: ~p~n---------
>>>>>>>~n~n",
	[SortedTE]).
	

test_c45(Options) ->
	%% get Attributes and TrainingExamples 
	Attributes = atts(),
	Class = class(),
	TrainingExamples = te(),
		

	{ok, Classifier} = 
		mllib:learn(Attributes, Class, TrainingExamples, c45, [{test_choice, information_gain_criterium} | Options]),

	%% transform
	{NewAttributes, Transformer} = mllib:transform_attributes(discrete, Attributes, Class, TrainingExamples, [{name, temp}] ++ Options),
	NewTE = [ mllib:transform_example(Transformer, TE) || TE <- TrainingExamples ],

	{ok, TClassifier} = 
		mllib:learn(NewAttributes, Class, NewTE, c45, [{test_choice, information_gain_criterium} | Options]),
	
	io:format("<><><> Comparision <><><>~n"),
	io:format("<><><> Normal <><><>~n"),	
	perform_classify_test(Attributes, Classifier, TrainingExamples, 0, 0, 0),
	io:format("<><><> Continuous transformed <><><>~n"),
	perform_classify_test(NewAttributes, TClassifier, NewTE, 0, 0, 0).


perform_classify_test(Attributes, Classifier, [{Example, ActualCategory}|T], Correct, Incorrect, Errors) ->
	Options = [],
	case mllib:classify(Classifier, Example, Options) of
		{ok, ActualCategory} -> perform_classify_test(Attributes, Classifier, T, Correct + 1, Incorrect, Errors);
		{ok, _} -> perform_classify_test(Attributes, Classifier, T, Correct, Incorrect + 1, Errors);
		{error, _} -> perform_classify_test(Attributes, Classifier, T, Correct, Incorrect, Errors + 1)
	end;
perform_classify_test(_Attributes, _Classifier, [], Correct, Incorrect, Errors) ->
	io:format("Correct: ~p, Incorrect: ~p, Error: ~p~n=========~n", [Correct, Incorrect, Errors]),	
	{Correct, Incorrect, Errors}.



