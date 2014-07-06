-module(arff).
-export([file/1]).

file(Filename) ->
	case file:read_file(Filename) of
		{ok, Bin} -> scan_parse([], binary_to_list(Bin), 0, []);
		{error, _} -> {error, parse_error}
	end.

scan_parse(Cont, Str, StartLoc, Acc) ->
	case arff_scanner:tokens(Cont, Str, StartLoc) of
		{done, {ok, Tokens, EndLoc}, LeftOverChars} ->
			case arff_parser:parse(Tokens) of
				{ok, Form} -> scan_parse([], LeftOverChars, EndLoc, [Form|Acc]);
				_ -> {error, parse_error}
			end;
		_ -> lists:reverse(Acc)
	end.