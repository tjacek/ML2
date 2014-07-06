%% Author: matis
%% Created: 2011-04-13
%% Description: TODO: Add description to tester
-module(tester).

%%
%% Include files
%%
-include("include/definitions.hrl").
-include("structures.hrl").
-include("debug.hrl").

%%
%% Exported Functions
%%
-export([test/2, test2/2]).

%%
%% API Functions
%%

% Zwraca tuple {A, B, C}:
% A - ilosc poprawnie sklasyfikowanych przykladow
% B - ilosc niepoprawnie sklasyfikowanych przykladow
% C - ilosc bledow, ktore wystapily podczas klasyfikacji
test(Algorithm, Options) ->
	TrainingExamples = [
						{{sloneczna, ciepla, duza, slaby}, false},
						{{sloneczna, ciepla, duza, silny}, false},
						{{pochmurna, ciepla, duza, slaby}, true},
						{{deszczowa, umiarkowana, duza, slaby}, true},
						{{deszczowa, zimna, normalna, slaby}, true},
						{{deszczowa, zimna, normalna, silny}, false},
						{{pochmurna, zimna, normalna, silny}, true}					   
					   ],
	Examples = [
						{{sloneczna, umiarkowana, duza, slaby}, false},
						{{sloneczna, zimna, normalna, slaby}, true},
						{{deszczowa, umiarkowana, normalna, slaby}, true},
						{{sloneczna, umiarkowana, normalna, silny}, true},
						{{pochmurna, umiarkowana, duza, silny}, true},
						{{pochmurna, ciepla, normalna, slaby}, true},
						{{deszczowa, umiarkowana, duza, silny}, false}
			   ],
	perform_test(?Attributes, ?Concept, TrainingExamples, Examples, Algorithm, Options).


test2(Algorithm, Options) ->
	TrainingExamples = [
						{{sloneczna, ciepla, duza, slaby}, false},
						{{sloneczna, ciepla, duza, silny}, false},
						{{pochmurna, ciepla, duza, slaby}, true},
						{{deszczowa, umiarkowana, duza, slaby}, true},
						{{deszczowa, zimna, normalna, slaby}, true},
						{{deszczowa, zimna, normalna, silny}, false},
						{{pochmurna, zimna, normalna, silny}, true},
						{{sloneczna, umiarkowana, duza, slaby}, false},
						{{sloneczna, zimna, normalna, slaby}, true},
						{{deszczowa, umiarkowana, normalna, slaby}, true},
						{{sloneczna, umiarkowana, normalna, silny}, true},
						{{pochmurna, umiarkowana, duza, silny}, true},
						{{pochmurna, ciepla, normalna, slaby}, true},
						{{deszczowa, umiarkowana, duza, silny}, false}
			   ],
	perform_test(?Attributes, ?Concept, TrainingExamples, TrainingExamples, Algorithm, Options).


%%
%% Local Functions
%%

perform_test(Attributes, Class, TrainingExamples, Examples, Algorithm, Options) ->
	{ok, Classifier} = mllib:learn(Attributes, Class, TrainingExamples, Algorithm, Options),
	perform_classify_test(Attributes, Classifier, Algorithm, Examples, 0, 0, 0).

perform_classify_test(Attributes, Classifier, Algorithm, [{Example, ActualCategory}|T], Correct, Incorrect, Errors) ->
	Options = [],
	case Algorithm:classify(Classifier, Example, Options) of
		{ok, ActualCategory} -> perform_classify_test(Attributes, Classifier, Algorithm, T, Correct + 1, Incorrect, Errors);
		{ok, _} -> perform_classify_test(Attributes, Classifier, Algorithm, T, Correct, Incorrect + 1, Errors);
		{error, _} -> perform_classify_test(Attributes, Classifier, Algorithm, T, Correct, Incorrect, Errors + 1)
	end;
perform_classify_test(_Attributes, _Classifier, _Algorithm, [], Correct, Incorrect, Errors) ->
	{Correct, Incorrect, Errors}.

