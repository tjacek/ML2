%% Author: matis
%% Created: 2011-04-11
%% Description: TODO: Add description to ex_gen
-module(ex_gen).

%%
%% Include files
%%

-include("include/definitions.hrl").

%%
%% Exported Functions
%%
-export([generate_examples/1, generate_training_examples/1]).

%%
%% API Functions
%%

generate_examples(0) ->
	ok;
generate_examples(Num) ->
	ok.

generate_training_examples(0) ->
	ok;
generate_training_examples(Num) ->
	ok.



%%
%% Local Functions
%%

