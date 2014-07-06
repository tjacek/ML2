-module(port_checker).
-export([start/0, stop/0, init/0, convert/1]).
-export([print_structs/0]).

-include("structures.hrl").


start() ->
    spawn(?MODULE, init, []).
stop() ->
    port_checker ! stop.

convert(Msg) ->
    port_checker ! {call, self(), Msg},
    receive
	{port_checker, Result} ->
	    Result
    end.

init() ->
    register(port_checker, self()),
    process_flag(trap_exit, true),
    Port = open_port({spawn, "port/worker"}, [{packet, 2}, binary]),
    io:format("~p\n", [Port]),
    loop(Port).

loop(Port) ->
    receive
	{call, Caller, Msg} ->
	    Port ! {self(), {command, term_to_binary(Msg)}},
	    receive
		{Port, {data, Data}} ->
		    Caller ! {port_checker, binary_to_term(Data)}
	    end,
	    loop(Port);
	stop ->
	    Port ! {self(), close},
	    receive
		{Port, closed} ->
		    exit(normal)
	    end;
	{'EXIT', Port, Reason} ->
	    exit(port_terminated)
    end.

print_structs() ->
	{Attributes, TrainingExamples} = mllib:read(xml, [{attributes, "poker/poker.xml"}, {tes, "poker/poker.data"}]),
	{NewAttributes, Class, TestExamples} = mllib:choose_class(Attributes, TrainingExamples, hand),
	[First|_T] = TestExamples,
	io:format("~p\n~p\n~p\n", [NewAttributes, Class, First]).  



