Nonterminals relation_rule attribute_rule attribute_rules data_rule root nominal_values.
Terminals decl_relation decl_attribute decl_data id '{' '}' ','.
Rootsymbol root.

root -> relation_rule attribute_rules data_rule : '$2'.
relation_rule -> decl_relation id : {relation, value_of('$2')}.
attribute_rules -> attribute_rule attribute_rules : [{attribute, '$1'}|'$2'].
attribute_rules -> attribute_rule : [{attribute, '$1'}].
attribute_rule -> decl_attribute id id : {list_to_atom(value_of('$2')), list_to_atom(string:to_lower(value_of('$3')))}.
attribute_rule -> decl_attribute id '{' nominal_values '}' : {list_to_atom(value_of('$2')), '$4'}.
nominal_values -> id ',' nominal_values : [to_num_or_atom(value_of('$1')) | '$3'].
nominal_values -> id : [to_num_or_atom(value_of('$1'))].
data_rule -> decl_data : data.

Erlang code.

value_of(Token) ->
	element(3, Token).

to_num_or_atom(Str) ->
	case string:to_float(Str) of
		{Float, []} -> Float;
		_ -> case string:to_integer(Str) of
				 {Int, []} -> Int;
				 _ -> list_to_atom(Str)
			 end
	end.
