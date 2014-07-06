-module(plot).

%%
%% Exported Functions
%%
-export([start/0, generate_chart/1, loop/3]).


start() ->
    {ok, Fd} = file:open("gnuplot_script.plt", write),
	Pid = spawn(plot, generate_chart, [Fd]),
	register(chart, Pid),
	read(Fd),
    {ok}.
	
read(Fd) ->
	{ok, NodesList} = file:consult("chart_data.txt"),
	read_loop(NodesList,Fd),
    {ok}.
	
read_loop([], Fd) ->
    chart ! {print, self()},
	receive
		{ok} -> 
			file:close(Fd)
	end;
read_loop([{NodeNumber, Time}|T], Fd) ->
    chart ! {addTime, NodeNumber, Time},
    read_loop(T, Fd).
    
generate_chart(Fd)->
	loop(Fd, [], []).
	
loop(Fd, Names, Times) ->
	receive
		{addTime, NodeNumber, Time} ->
			NewNames=[NodeNumber|Names],
			NewTimes=[Time|Times],
			loop(Fd, NewNames, NewTimes);
		{print, Pid} ->
			io:format(Fd, "set terminal jpeg ~nset output 'chart.jpg' ~nset nokey ~nset yrange [0:] ~nset xrange [0:~p] ~nset grid ~nset xtics (",[length(Names)+1]),
			write_names(Fd, lists:reverse(Names), 1),
			io:format(Fd, "plot '<echo ", []),
			Number = write_times(Fd, lists:reverse(Times), 1),
			write_end_of_plot(Fd, Number, 1),
			os:cmd("gnuplot gnuplot_script.plt"),
			Pid ! {ok}
	end,
	{ok, chart}.
	
write_names(Fd, [NodeNumber], Number) ->
	io:format(Fd, "'~p' ~p)~n", [NodeNumber, Number]);
write_names(Fd, [NodeNumber|Rest], Number) ->
	io:format(Fd, "'~p' ~p, ", [NodeNumber, Number]),
	write_names(Fd, Rest, Number+1).

write_times(Fd, [Time], Number) ->
	io:format(Fd, "~p ~p' ", [Number, Time]),
	Number;
write_times(Fd, [Time|Rest], Number) ->
	io:format(Fd, "~p ~p ", [Number, Time]),
	write_times(Fd, Rest, Number+1).

write_end_of_plot(Fd, EndNumber, Number) ->
	case Number of
		EndNumber ->
			io:format(Fd, "u ~p:~p w imp lw 20~n", [2*Number-1, 2*Number]);
		_ ->
			io:format(Fd, "u ~p:~p w imp lw 20, '' ", [2*Number-1, 2*Number]),
			write_end_of_plot(Fd, EndNumber, Number+1)
	end.

