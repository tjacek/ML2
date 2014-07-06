-module(distc45).
-export([test/0, test/1, test2/1, testn/1]).

-include("structures.hrl").

test() ->
	{Attributes, TrainingExamples} = mllib:read(xml, [{attributes, "poker/poker.xml"}, {tes, "poker/poker.data"}]),

	{Distributed, _Res} = timer:tc(mllib, learn_debug, [Attributes, hand, TrainingExamples, c45single, []]),
	io:format("Time: ~p\n", [Distributed]).

test(Nodes) ->
	{Attributes, TrainingExamples} = mllib:read(xml, [{attributes, "poker/poker.xml"}, {tes, "poker/poker.data"}]),

	Options = [{test_choice, information_gain_criterium}],
	%Options = [{test_choice, information_growth_factor_criterium}],
	{NewAttributes, Class, NewTrainingExamples} = mllib:choose_class(Attributes, TrainingExamples, hand),

	%{Single, _Res} = timer:tc(c45single, learn, [NewAttributes, Class, NewTrainingExamples, Options]),
	%io:format("Not distributed: ~p\n", [Single]),
	{Distributed, _Res1} = timer:tc(mllib, learn, [Attributes, hand, TrainingExamples, c45, Options, Nodes]),
	io:format("Distributed: ~p\n", [Distributed]).

testn(Nodes) ->
	{Attributes, TrainingExamples} = mllib:read(xml, [{attributes, "poker/poker.xml"}, {tes, "poker/poker_ss.data"}]),
	{NewAttributes, Class, TestExamples} = mllib:choose_class(Attributes, TrainingExamples, hand),
	Algorithm = naive_bayes,

	%{Distributed, _Res1} = timer:tc(mllib, learn, [Attributes, hand, TrainingExamples, naive_bayes, [], Nodes]),
	{ok, Res} = mllib:learn(Attributes, hand, TrainingExamples, Algorithm, [], Nodes),
		
	%io:format("Distributed: ~p\n", [Distributed]),
	io:format("Res: ~p\n", [Res]).

	%perform_classify_test(Attributes, Res, Algorithm, TestExamples, 0, 0, 0).

perform_classify_test(Attributes, Classifier, Algorithm, [{Example, ActualCategory}|T], Correct, Incorrect, Errors) ->
	Options = [],
	case Algorithm:classify(Classifier, Example, Options) of
		{ok, ActualCategory} -> perform_classify_test(Attributes, Classifier, Algorithm, T, Correct + 1, Incorrect, Errors);
		{ok, _} -> perform_classify_test(Attributes, Classifier, Algorithm, T, Correct, Incorrect + 1, Errors);
		{error, _} -> perform_classify_test(Attributes, Classifier, Algorithm, T, Correct, Incorrect, Errors + 1)
	end;
perform_classify_test(_Attributes, _Classifier, _Algorithm, [], Correct, Incorrect, Errors) ->
	io:format("Correct: ~p, Incorrect: ~p, Error: ~p~n=========~n", [Correct, Incorrect, Errors]),	
	{Correct, Incorrect, Errors}.



test2(Nodes) ->
	{_A, _C, TrainingExamples} = mllib:read(c45, [{attributes, "car/car.c45-names"}, {tes, "car/car.data"}]),

	Attributes = [{attribute,buying,nominal,[vhigh,high,med,low]},
			{attribute,maint,nominal,[vhigh,high,med,low]},
			{attribute,doors,nominal,[2,3,4,more5]},
			{attribute,persons,nominal,[2,4,more]},
			{attribute,lug_boot,nominal,[small,med,big]},
			{attribute,safety,nominal,[low,med,high]}],

	%Options = [{test_choice, information_gain_criterium}],
	Options = [{test_choice, information_growth_factor_criterium}],

	Class = {class,class,[],[unacc,acc,good,vgood]},
	%{Single, _Res} = timer:tc(mllib, learn, [Attributes, Class, TrainingExamples, c45single, Options, Nodes]),
	%io:format("Not distributed: ~p\n", [Single]),
	Single = 0,
	{Distributed, _Res1} = timer:tc(mllib, learn, [Attributes, Class, TrainingExamples, c45nonu, Options, Nodes]),
	io:format("Not distributed: ~p, Distributed: ~p\n", [Single, Distributed]).




% make:all([load]), distc45:test2([d1@Deski, d2@Deski]).


