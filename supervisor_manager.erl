-module(supervisor_manager).

%%
%% Behaviour files
%%
-behaviour(supervisor).
-export([start_link/3]).
-export([init/1, create_event/4, event_loop/4, compute/2, message/2]).
-export([start_ets_local/1, connect_local/1]).


start_link({Module, Func, Args, no_ets}, Nodes, ReturnPid) ->
    supervisor:start_link({local, supervisor_actual}, supervisor_manager, {Module, Func, Args, Nodes, ReturnPid, no_ets});
start_link({Module, Func, Args}, Nodes, ReturnPid) ->
    supervisor:start_link({local, supervisor_actual}, supervisor_manager, {Module, Func, Args, Nodes, ReturnPid}).


init({Module, Func, Args, Nodes, ReturnPid, no_ets}) -> % version without ets for naive bayes purposes
	register(node_provider, spawn(load_balancer, node_provider, [Nodes])),
	connect_all(length(Nodes)),
	Pid = spawn(supervisor_manager, create_event, [Module, ReturnPid, [], self()]),
	Child = {0, {load_balancer, start, [Module, Func, Args, Pid, 0]}, transient, brutal_kill, worker, [load_balancer]},
	receive
		ok -> {ok, {{rest_for_one, 1, 10}, [Child]}}
	end;
init({Module, Func, [A1, A2, TES, A4], Nodes, ReturnPid}) -> % version with ets tables
	register(node_provider, spawn(load_balancer, node_provider, [Nodes])), % start load balancer
	connect_all(length(Nodes)), % wait for all Nodes to start
	EnumeratedTES = enumerate(TES, [], 1), % assign id no to each training example
	EtsPids = start_ets(EnumeratedTES, length(Nodes)), % start ets on each node
	Pid = spawn(supervisor_manager, create_event, [Module, ReturnPid, EtsPids, self()]), % event_manager -> time tracking
	Child = {0, {load_balancer, start, [Module, Func, [A1, A2, EnumeratedTES, A4], Pid, 0]}, transient, brutal_kill, worker, [load_balancer]},
	receive
		ok -> {ok, {{rest_for_one, 1, 10}, [Child]}}
	end;
init({Module, Func, [Data, Options], Nodes, ReturnPid}) -> % version without ets tables
	register(node_provider, spawn(load_balancer, node_provider, [Nodes])),
	connect_all(length(Nodes)),
	Pid = spawn(supervisor_manager, create_event, [Module, ReturnPid, [], self()]),
	Child = {0, {load_balancer, start, [Module, Func, [Data, Options], Pid, 0]}, transient, brutal_kill, worker, [load_balancer]},
	receive
		ok -> {ok, {{rest_for_one, 1, 10}, [Child]}}
	end.

% make:all([load]), {A, C, T} = mllib:read(c45, [{tes, "example_data_c45"}, {attributes, "example_c45"}]), mllib:learn(A, C, T, example_computation, [], [d1@Deski]), f().
% make:all([load]), {A, C, T} = mllib:read(xml, [{tes, "example_data"}, {attributes, "example_attributes.xml"}]), mllib:learn(A, aura, T, example_computation, [], [d1@Deski]), f().

create_event(Module, ReturnPid, EtsPids, ConfirmationPid) ->
	gen_event:start({local, nodes_time}),
	gen_event:add_handler(nodes_time, event_manager, []),
	case global:register_name(supervisor, self()) of
		yes -> ConfirmationPid ! ok, event_loop(Module, 1, ReturnPid, EtsPids);
		no -> io:format("Cannot register 'supervisor'~n", []), exit(cannot_register_name)
	end.

event_loop(Module, N, ReturnPid, EtsPids) ->
	receive 
		{_OtherProcess, {time, NodeName, Time}} ->
			gen_event:notify(nodes_time, {NodeName, Time, options}),
			event_loop(Module, N, ReturnPid, EtsPids);
		{_OtherProcess, stop} ->
			gen_event:stop(nodes_time),
			node_provider ! {self(), stop},
			unregister(node_provider),
			global:unregister_name(supervisor),
			ReturnPid ! {ok, stopped},
			stop_ets(EtsPids),
			ok;
		{OtherPid, {compute, Func, Args}} ->
			Child = {N, {load_balancer, start, [Module, Func, Args, OtherPid, N]}, transient, brutal_kill, worker, [load_balancer]},
			case supervisor:start_child(supervisor_actual, Child) of
				{ok, _} -> OtherPid ! {ok, N}, event_loop(Module, N + 1, ReturnPid, EtsPids);
				{error, _} -> OtherPid ! {error, cannot_start_child}, ReturnPid ! {error, cannot_start_child}, error
			end;
		{OtherPid, {get_child_pid, Id}} ->
			Children = supervisor:which_children(supervisor_actual),
			OtherPid ! get_child_pid_from_list(Children, Id),
			event_loop(Module, N, ReturnPid, EtsPids);
		{result, 0, Result} -> % root zwrocil wynik
			gen_event:stop(nodes_time),
			node_provider ! {self(), stop},
			unregister(node_provider),
			global:unregister_name(supervisor),
			ReturnPid ! {ok, Result},
			stop_ets(EtsPids),
			ok
	end.

% retrieve pid of the child with specified id from the children list
get_child_pid_from_list([{Id, Pid, _, _}|_], Id) ->
	{ok, Pid};
get_child_pid_from_list([_|T], Id) ->
	get_child_pid_from_list(T, Id);
get_child_pid_from_list([], _) ->
	{error, child_not_found}.

%%%%%%%%%%%%%%%%%%%%%
% functions meant to be called from within algorithms
%%%%%%%%%%%%%%%%%%%%%

% send message to event loop to start new child
compute(Func, Args) ->
	global:send(supervisor, {self(), {compute, Func, Args}}),
	receive
		{ok, Id} -> Id;
		{error, Reason} -> exit(Reason) 
	end.

% send message to a child with specified id
message(Id, Message) ->
	global:send(supervisor, {self(), {get_child_pid, Id}}),
	receive
		{ok, undefined} -> {error, child_not_running};
		{ok, Pid} -> Pid ! {self(), Message}, ok;
		{error, Reason} -> {error, Reason}
	end.

%%%%%%%%%%%%%%%%%%%%%
% ETS
%%%%%%%%%%%%%%%%%%%%%


% start hashtable processes on all nodes
start_ets(EnumeratedTES, NumOfNodes) ->
	start_ets_rec(EnumeratedTES, NumOfNodes, []).

start_ets_rec(_EnumeratedTES, 0, Pids) ->
	Pids;
start_ets_rec(EnumeratedTES, NumOfNodes, Pids) ->
	node_provider ! {self(), node_request},
	receive
		{node_response, Node} -> Pid = spawn_link(Node, ?MODULE, start_ets_local, [EnumeratedTES]), start_ets_rec(EnumeratedTES, NumOfNodes - 1, [Pid|Pids])
	end.

start_ets_local(EnumeratedTES) ->
	ets:new(tes_table, [set, public, named_table]),
	ets:insert(tes_table, EnumeratedTES),
	receive
		stop ->	ok
	end.

enumerate([TE|Tail], Acc, N) ->
	enumerate(Tail, [{N, TE}|Acc], N + 1);
enumerate([], Acc, _) ->
	lists:reverse(Acc).

% stop hashtable processes
stop_ets([]) ->
	ok;
stop_ets([Pid|EtsPids]) ->
	Pid ! stop,
	stop_ets(EtsPids).

% connect all nodes
connect_all(0) ->
	ok;
connect_all(Num) ->
	node_provider ! {self(), node_request},
	receive
		{node_response, Node} -> spawn(Node, ?MODULE, connect_local, [self()]),
								 receive
									 ok -> connect_all(Num - 1)
								 end
	end.

connect_local(Pid) ->
	Pid ! ok,
	ok.
