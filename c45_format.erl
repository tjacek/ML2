-module(c45_format).
-export([file/1]).

file(Filename) ->
	case file:read_file(Filename) of
		{ok, Bin} -> scan_parse(string:strip(binary_to_list(Bin), both, $\n));
		{error, Reason} -> {error, Reason}
	end.

scan_parse(Str) ->
	case c45_parser:parse(element(2, c45_scanner:string(Str))) of
		{ok, Parsed} -> Parsed;
		{error, Reason} -> {error, Reason}
	end.
	

% debugger:quick(c45_format, file, ["example_c45"]).