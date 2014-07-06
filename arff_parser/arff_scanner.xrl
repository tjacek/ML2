Definitions.

NQ_ID = ([a-zA-Z0-9]|-|_|\.)+
WS = [\000-\s]

Rules.

@(r|R)(e|E)(l|L)(a|A)(t|T)(i|I)(o|O)(n|N) : {token, {decl_relation, TokenLine}}.
@(a|A)(t|T)(t|T)(r|R)(i|I)(b|B)(u|U)(t|T)(e|E) : {token, {decl_attribute, TokenLine}}.
@(d|D)(a|A)(t|T)(a|A)[.\n]* : {end_token, {decl_data, TokenLine}}.

{NQ_ID} : {token, {id, TokenLine, TokenChars}}.
'({WS}*{NQ_ID}{WS}*)+' : {token, {id, TokenLine, string:strip(TokenChars, both, $\')}}.
'' : {error, "Empty string"}.
\{ : {token, {'{', TokenLine}}.
\} : {token, {'}', TokenLine}}.
\, : {token, {',', TokenLine}}.
{WS}+ : skip_token.
\%.* : skip_token.

Erlang code.
