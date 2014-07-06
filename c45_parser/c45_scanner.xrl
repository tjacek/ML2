Definitions.

% Pytajnik, przecinek i dwukropek nie dzialaja 

%NQ_ID = ([a-zA-Z]|-|_)([a-zA-Z0-9]|-|_)*
NQ_ID = ([a-zA-Z0-9]|-|_)+
ESCAPED = \\\?|\\\,|\\\:
WS = \s
NL = \n
CR = \r

Rules.

%% Nie wiadomo dlaczego, ale to nie dziala :-(
continuous : {token, {continuous, TokenLine}}.
discrete : {token, {discrete, TokenLine}}.
ignore : {token, {ignore, TokenLine}}.
%%

{WS}+ : skip_token.
'' : {error, "Empty string"}.
\, : {token, {',', TokenLine}}.
\. : {token, {'.', TokenLine}}.
\: : {token, {':', TokenLine}}.
({CR}|{NL}|{CR}{NL}) : {token, {br, TokenLine}}.
\|.* : skip_token.
({NQ_ID}|{ESCAPED})+({WS}|{NQ_ID}|{ESCAPED})* : check_token(string:strip(TokenChars), TokenLine).
[0-9]+ : {token, {int, TokenLine, list_to_integer(TokenChars)}}.

Erlang code.

check_token("continuous", TokenLine) ->
	{token, {continuous, TokenLine}};
%check_token("discrete", TokenLine) ->
%	{token, {discrete, TokenLine}};
check_token("ignore", TokenLine) ->
	{token, {ignore, TokenLine}};
check_token(Str, TokenLine) ->
	case re:run(Str, "\s*discrete\s+([0-9])+", [{capture, all_but_first, list}]) of
		{match, [Value]} -> {token, {int, TokenLine, list_to_integer(Value)}};
		nomatch -> {token, {id, TokenLine, lists:filter(fun(X) -> X /= 92 end, re:replace(Str, "\s+", " ", [{return, list}, global]))}}
	end.
