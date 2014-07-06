%% Author: matis
%% Created: 04-11-2011
%% Description: TODO: Add description to example_computation
-module(example_computation).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([myfun/1, learn/4]).

%%
%% API Functions
%%

learn(_, _, _, _) ->
	myfun(3).

myfun(0) ->
	timer:sleep(2000),
	1;
myfun(N) ->
	io:format("Computing: ~p on node: ~p. Ets test: ~p~n", [N, node(), ets:lookup(tes_table, 1)]),
	timer:sleep(1000),
	Id1 = supervisor_manager:compute(myfun, [N - 1]),
	Id2 = supervisor_manager:compute(myfun, [N - 1]),
	io:format("   Waiting for ~p~n", [Id1]),
	FirstResult = receive {result, Id1, Res1} -> Res1 end,
	io:format("   Got ~p~n", [Id1]),
	io:format("   Waiting for ~p~n", [Id2]),
	SecondResult = receive {result, Id2, Res2} -> Res2 end,
	io:format("   Got ~p~n", [Id2]),
	FirstResult + SecondResult.

%%
%% Local Functions
%%

