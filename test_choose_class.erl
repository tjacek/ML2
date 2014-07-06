-module(test_choose_class).

%%
%% Include files
%%
-include("structures.hrl").
-include("debug.hrl").

%%
%% Exported Functions
%%
-export([test/0]).

%%
%% API Functions
%%


atts() ->
	[#attribute{name = nom, type = nominal, values = [a,b,c]},
	#attribute{name = temp, type = continuous, values = {-100, 100}},
	#attribute{name = ord, type = ordered, values = [a1, a2, a3]},
	#attribute{name = three, type = nominal, values = [x, y, z]}].

te() ->
	[[a, -2, a3, x],
	[a, -11, a2, x],
	[b, -12, a1, x],
	[a, 5, a2, y],
	[a, 8, a2, y],
	[b, 12, a1, y],
	[b, 17, a2, z],
	[a, 22, a1, z],
	[c, -11, a2, x],
	[a, 20, a1, y],
	[a, 22, a2, z],
	[c, 5, a3, y],
	[c, 1, a2, y]].

test() ->
	Attributes = atts(),
	TrainingExamples = te(),

	io:format("===  ===
TrainingExamples: ~p~n-------
Attributes: ~p~n--------~n", [TrainingExamples, Attributes]),

	{NewAttributes, Class, NewTEs} = mllib:choose_class(Attributes, TrainingExamples, three),

	io:format("=== Choose three =========
Class: ~p~n----------
TrainingExamples: ~p~n---------
Attributes: ~p
>>>>>>>~n",
	[Class, NewTEs, NewAttributes]).
















