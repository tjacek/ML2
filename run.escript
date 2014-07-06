
main([DWorkers, CWorkers]) ->
	main([DWorkers, CWorkers, "0.002", "billing.txt"]);
main([DWorkers, CWorkers, Support]) ->
	main([DWorkers, CWorkers, Support, "billing.txt"]);
main([DWorkers, CWorkers, Support, File]) ->
	make:all([load]),
	{DW,_} = string:to_integer(DWorkers),
	{CW,_} = string:to_integer(CWorkers),
	{Sup,_} = string:to_float(Support),
	{Time, ok} = timer:tc(apriori_test, work, [apriori, DW, CW, Sup, File]),
	io:format("~p\n", [Time]);
main(_) ->
	io:format("Usage : DataSplit CandidatesSplit [Support=0.002] [File=billing.txt]~n").
	