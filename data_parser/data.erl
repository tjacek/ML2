-module(data).
-export([file/1]).

file(Filename) ->
	case file:open(Filename, [read]) of
		{ok, IODevice} -> parse_all_lines(IODevice, []);
		{error, _} -> {error, parse_error}
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
