-module(mllib_manager).
-export([gendoc/0, runtest/0, make_debug/0, cake_debug/0]).

-spec gendoc() -> ok.
% @doc Generates this documentation
% @spec gendoc() -> ok
gendoc() ->
	edoc:packages([""], [{dir, doc}]),
	ok.

runtest() ->
	c:cd("test"),
	{_State, File} = file:open(module_dirs, [read]),
	add_search_paths( readPaths(File, []) ),
	file:close(File),

	ok = ct:run_test({spec, "testspec"}),

	c:cd("..").

make_debug() ->
	make:all([load, debug_info, {d, 'DEBUG'}]).

cake_debug() ->
	make:all([load, debug_info]).


readPaths(File, Paths) ->
	case file:read_line(File) of
		eof -> Paths;
		{ok, Line} -> readPaths(File, [make_path(cut_newline(Line)) | Paths])
	end.

make_path(RelativePath) ->
	{_State, Cwd} = file:get_cwd(),
	Cwd ++ "/" ++ RelativePath.

cut_newline(String) ->
	re:replace(String, "\n", "",  [{return, list}]). 

add_search_paths([Path | T]) ->
	code:add_patha(Path),
	add_search_paths(T);
add_search_paths([]) ->
	ok.


