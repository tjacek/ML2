-ifndef(MLLIB_STRUCTURES).
-define(MLLIB_STRUCTURES, ok).

% type = nominal | ordered | continuous
% case type of
%     nominal -> values = [Val1, Val2, ...];
%     ordered -> values = [Val1, Val2, ...]; % order relation Val1 < Val2
%     continuous -> values = {A, B}
% end.
-record(attribute, {name, type, values}).

% if class is binary -> categories=[true, false]
-record(class, { name, description="", categories=[]}).

% module name
-record(classifier, { algorithm, attributes, class, specific_classifier }).

-type name() :: atom() | integer().
% @type name() = atom() | integer().

-type value() :: atom() | integer() | float().
% @type value() = atom() | integer() | float().

-type attribute_type() :: nominal | ordered | continuous.
% @type attribute_type() = nominal | ordered | continuous.

-type attribute() :: #attribute{name :: name(), type :: attribute_type(), values :: [value()]}.
% @type attribute() = #attribute{name = name(), type = attribute_type(), values = [value()] | {number(), number()}}. Case type of<br/>nominal -> values = [value()];<br/>ordered -> values = [value()]; % order relation ``Val1 < Val2'';<br/>continuous -> values = {number(), number()}

-type category() :: name().
% @type category() = name().

-type class() :: #class{ name :: name(), description :: string(), categories :: [category()]}.
% @type class() = #class{ name = name(), description = string(), categories = [category()]}.

-type classifier() :: #classifier{ algorithm :: atom(), class :: class(), specific_classifier :: any()}.
% @type classifier() = #classifier{ algorithm = atom(), class = class(), specific_classifier = any()}.

-type example() :: tuple().
% @type example() = tuple(). Should be of length length(Attributes).

-type training_example() :: {example(), category()}.
% @type training_example() = {example(), category()}.

-record(transformer, {type, data, t_fun}).
-type transformer() :: #transformer{ type :: atom(), data :: any(), t_fun :: fun( ) }.
% @type class() = #transformer{ type = atom(), data = any(), t_fun = fun()}.


-type empty() :: any().
% @type empty() = any().

-endif.


