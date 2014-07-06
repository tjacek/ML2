-module(data).
-export([file/1, file_arff/1]).

file(Filename) ->
	case file:open(Filename, [read]) of
		{ok, IODevice} -> parse_all_lines(IODevice, []);
		{error, _} -> {error, parse_error}
	end.

file_arff(Filename) ->
	case file:open(Filename, [read]) of
		{ok, IODevice} -> parse_all_lines(skip_until_data(IODevice), []);
		{error, _} -> {error, parse_error}
	end.

skip_until_data(IODevice) ->
	{ok, Line} = file:read_line(IODevice),
	case string:strip(string:to_lower(Line)) of
		"@data\n" -> IODevice;
		_ -> skip_until_data(IODevice)
	end.		

parse_all_lines(IODevice, Acc) ->
	case file:read_line(IODevice) of
		eof -> Acc;
		{ok, Line} ->
			case Line of
				"\n" -> parse_all_lines(IODevice, Acc);
				_ -> {ok, Tokens, _} = data_scanner:string(Line), {ok, [Result]} = data_parser:parse(Tokens), parse_all_lines(IODevice, [Result|Acc])
			end;
		X -> X
	end.
