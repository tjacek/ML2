-module(event_manager).

%%
%% Behaviour files
%%
-behaviour(gen_event).

%%
%% Exported Functions
%%
-export([init/1, handle_event/2, terminate/2, code_change/3, handle_info/2, handle_call/2]).


init(_InitArgs) ->
    {ok, Fd} = file:open("chart_data.txt", write),
    {ok, Fd}.
	
handle_event({NodeName, Time, _Options}, Fd) ->
	io:format(Fd, "{~p, ~p}.~n", [NodeName, Time]),
    {ok, Fd}.
	
terminate(_Args, Fd) ->
  flie:close(Fd).
	
code_change(_OldVsn, _State, _Extra) -> 
	{ok, newState}.
	
handle_info(_Info, _State) -> 
	{ok, newState}.
	
handle_call(_Request, _State) -> 
	{ok, newState}.	
