Nonterminals root classes class_elements attribute_rules attribute_rule attribute_rule_outer nominal_values brs.
Terminals id ',' '.' ':' br continuous ignore int.
Rootsymbol root.

root -> brs classes attribute_rules brs : '$3' ++ [{class, '$2'}].

brs -> br brs.
brs -> '$empty'.

classes -> brs class_elements '.' : '$2'.
classes -> brs class_elements br : '$2'.
class_elements -> id : [list_to_atom(value_of('$1'))].
class_elements -> id ',' class_elements : [list_to_atom(value_of('$1'))|'$3'].

attribute_rules -> attribute_rule_outer : ['$1'].
attribute_rules -> attribute_rule_outer attribute_rules : ['$1'|'$2'].

attribute_rule_outer -> brs attribute_rule '.' : {attribute, '$2'}.
attribute_rule_outer -> brs attribute_rule br : {attribute, '$2'}.

attribute_rule -> id ':' continuous : {list_to_atom(value_of('$1')), continuous}.
attribute_rule -> id ':' int : {list_to_atom(value_of('$1')), generate_discrete_values(value_of('$3'))}.
attribute_rule -> id ':' nominal_values : {list_to_atom(value_of('$1')), '$3'}.
attribute_rule -> id ':' ignore : {list_to_atom(value_of('$1')), ignored}.

nominal_values -> id : [to_num_or_atom(value_of('$1'))].
nominal_values -> id ',' nominal_values : [to_num_or_atom(value_of('$1')) | '$3'].

Erlang code.

value_of(Token) ->
	element(3, Token).


generate_discrete_values(Num) when Num > 0 ->
	generate_discrete_values(Num, []).

generate_discrete_values(1, Acc) ->
	[value1|Acc];
generate_discrete_values(Num, Acc) ->
	generate_discrete_values(Num - 1, [list_to_atom(string:concat("value", integer_to_list(Num)))|Acc]).

to_num_or_atom(Str) ->
	case string:to_float(Str) of
		{Float, []} -> Float;
		_ -> case string:to_integer(Str) of
				 {Int, []} -> Int;
				 _ -> list_to_atom(Str)
			 end
	end.
