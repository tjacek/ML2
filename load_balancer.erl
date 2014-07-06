%% Author: matis
%% Created: 04-11-2011
%% Description: TODO: Add description to load_balancer
-module(load_balancer).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([start/5, start_local/5, node_provider/1]).

%%
%% API Functions
%%

node_provider([Node|T]) ->
	receive
		{Pid, node_request} ->
			Pid ! {node_response, Node},
			node_provider(T ++ [Node]);
		{_Pid, stop} ->
			ok
	end.

start(Module, Func, Args, RequestingPid, Id) ->
	node_provider ! {self(), node_request},
	receive
		{node_response, Node} ->
			Pid = spawn_link(Node, load_balancer, start_local, [Module, Func, Args, RequestingPid, Id]),
			{ok, Pid}
	end.

start_local(Module, Func, Args, RequestingPid, Id) ->
	{Time, Result} = timer:tc(Module, Func, Args),
	global:send(supervisor, {self(), {time, node(), Time}}),
	RequestingPid ! {result, Id, Result}.

%%
%% Local Functions
%%

