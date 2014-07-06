-module(tester_golf).
-export([test/0, test_c45/0]).
-include("structures.hrl").

test() ->
	%% get Attributes and TrainingExamples 
	{Attributes, TrainingExamples} = 
		mllib:read(xml, [{attributes, "attributes.xml"}, {tes, "training_examples.txt"}]),
		
	%% learn a "golf" class using c45 module
	ClassName = golf,
	{ok, Classifier} = 
		mllib:learn(Attributes, ClassName, TrainingExamples, c45, [ ]),
	
	%% serialize the Classifier
	ClassifierFilename = "golf_classifier",
	ok = mllib:write_classifier(ClassifierFilename, Classifier),
 
	%% uncomment this to get the Classifier from a file.
	% {ok, ClassifierFromFile} = mllib:read_classifier(ClassifierFilename),

	%% classify new Example
	Example = {sunny, warm, normal, strong},
	%% change "Classifier" to "ClassifierFromFile" to use the serialized Classifier.
	{ok, Category} = mllib:classify(Classifier, Example, [ ]),
 
	%% print the Category
	io:format("Obtained Category: ~p~n", [Category]).
%%=========================================================================================

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
	{{a, 20, a1}, z},
	{{a, 22, a2}, z},
	{{c, 5, a3}, y},
	{{c, 1, a2}, y}].

%%==========================================================================================

test_c45() ->
	%% get Attributes and TrainingExamples 
	Attributes = atts(),
	Class = class(),
	TrainingExamples = te(),
		

	{ok, Classifier} = 
		mllib:learn(Attributes, Class, TrainingExamples, c45, [ ]),

	perform_classify_test(Attributes, Classifier, c45, TrainingExamples, 0, 0, 0).

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

