Nonterminals root normal sparse value sparse_values sparse_value line.
Terminals known unknown int '{' '}' ','.
Rootsymbol root.

root -> line root: ['$1'|'$2'].
root -> line: ['$1'].
line -> normal : {normal, list_to_tuple('$1')}.
line -> sparse : {sparse, '$1'}.
normal -> value ',' normal : ['$1'|'$3'].
normal -> value : ['$1'].
sparse -> '{' sparse_values '}' : '$2'. %sparse_to_tuple('$2').
sparse_values -> sparse_value ',' sparse_values : ['$1'|'$3'].
sparse_values -> sparse_value : ['$1'].
sparse_value -> int value : {value_of('$1'), '$2'}.
value -> known : value_of('$1').
value -> int : value_of('$1').
value -> unknown : '?'.

Endsymbol newline.

Erlang code.

value_of(Token) ->
	element(3, Token).

%sparse_to_tuple(List) ->
%	list_to_tuple(construct_full_list(0, sort(ordering_function, List), [])).

%ordering_function({A, _}, {B, _}) when A =< B ->
%	true;
%ordering_function({_, _}, {_, _}) ->
%	false.

%construct_full_list(CurrentElement, [{Num, _} = H|T], Acc) when Num > CurrentElement ->
%	construct_full_list(CurrentElement + 1, [H|T], [0|Acc]);
%construct_full_list(CurrentElement, [{_, Val}|T], Acc) ->
%	construct_full_list(CurrentElement + 1, T, [Val|Acc]);