-module(apriori_test).
-export([test/3, work/5, work/4, work/2, work_b_prl/2]).

work(Module, DataWorkers, ItemWorkers, MinSup) ->
	{ok, Data} = mllib:read_mine_data("rest.txt"),
	{ok, Result} = mllib:mine(Data, Module, [{min_sup, MinSup}, {dataset_workers, DataWorkers}, {itemset_workers, ItemWorkers}]),
	case file:open("apriori_result", [write]) of
		{error, Reason} -> {error, Reason};
		{ok, IoDevice} -> io:fwrite(IoDevice, "~p", [Result]), file:close(IoDevice), ok
	end.

work(Module, DataWorkers, ItemWorkers, MinSup, File) ->
	{ok, Data} = mllib:read_mine_data(File),
	{ok, Result} = mllib:mine(Data, Module, [{min_sup, MinSup}, {dataset_workers, DataWorkers}, {itemset_workers, ItemWorkers}]),
	case file:open("apriori_result", [write]) of
		{error, Reason} -> {error, Reason};
		{ok, IoDevice} -> io:fwrite(IoDevice, "~p", [Result]), file:close(IoDevice), ok
	end.

work(DataWorkers, ItemWorkers) ->
	work(apriori, DataWorkers, ItemWorkers, 0.1).

work_b_prl(DataWorkers, ItemWorkers) ->
	work(apriori_b_prl, DataWorkers, ItemWorkers, 0.1).

test(DataWorkers, ItemWorkers, MinSup) ->
	AprioriTime = timer:tc(apriori_test, work, [apriori, DataWorkers, ItemWorkers, MinSup]),
	AprioriBPrlTime = timer:tc(apriori_test, work, [apriori_b_prl, DataWorkers, ItemWorkers, MinSup]),
	AprioriCyclicTime = timer:tc(apriori_test, work, [apriori_cyclic, DataWorkers, ItemWorkers, MinSup]),
	AprioriCombremotTime = timer:tc(apriori_test, work, [apriori_combremot, DataWorkers, ItemWorkers, MinSup]),
	io:format("Apriori : ~p\nAprioriBPrl : ~p\nAprioriCyclicTime : ~p\nAprioriCombremotTime : ~p\n", [AprioriTime, AprioriBPrlTime, AprioriCyclicTime, AprioriCombremotTime]).

