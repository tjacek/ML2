Definitions.

ID = [a-zA-Z_][a-zA-Z0-9_\-]*
%WS = [\000-\s]
WS = [\t\s]

Rules.

{ID} : {token, {known, TokenLine, list_to_atom(TokenChars)}}.
'({WS}*{ID}{WS}*)+' : {token, {known, TokenLine, list_to_atom(string:strip(TokenChars, both, $\'))}}.
\? : {token, {unknown, TokenLine}}.
(\+|-)?[0-9]+ : {token, {int, TokenLine, list_to_integer(TokenChars)}}.
(\+|-)?[0-9]+\.[0-9]+((E|e)(\+|-)?[0-9]+)? : {token, {known, TokenLine, list_to_float(TokenChars)}}.
'' : {error, "Empty string"}.
\{ : {token, {'{', TokenLine}}.
\} : {token, {'}', TokenLine}}.
\, : {token, {',', TokenLine}}.
{WS}+ : skip_token.
\%.* : skip_token.
\n : {end_token, {newline, TokenLine}}.

Erlang code.
