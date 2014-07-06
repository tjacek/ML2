-module(mllib).
-export([learn/5, learn/6, learn_naive/6, classify/3, transform_attributes/5, transform_example/2]).
-export([mine/4, mine/3, read_mine_data/1]).
-export([read/2, read/3, read_attributes/3, read_classes/3, read_examples/1, read_trainingexamples/1, write_classifier/2, read_classifier/1]).
-export([choose_class/3]).
-export([supervisor_spawner/5, supervisor_spawner/6]).
-include("structures.hrl").
-include("debug.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-define(RECOGNIZED_LEARN_OPTIONS, [no_check, attributes_ok, class_ok, trainingexamples_ok]).

% @headerfile "structures.hrl"


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%    NAIVE LEARN %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec learn_naive(Attributes :: [attribute()], Class :: class() | name(), TrainingExamples :: [training_example()], Algorithm :: atom(), Options :: [Option :: any()], Nodes :: [node()]) -> {ok, Classifier :: classifier()} | {error, Reason :: any()}.
% @spec learn_naive(Attributes, Class, TrainingExamples, Algorithm, Options, Nodes) -> {ok, Classifier} | {error, Reason} where Attributes = [attribute()], Class = class() | name(), TrainingExamples = [training_example()], Algorithm = atom(), Options = [Option], Option = any(), Nodes = [node()], Classifier = classifier(), Reason = any()
% @doc See learn for details. This won't start ets tables on nodes.
learn_naive(Attributes, #class{} = Class, TrainingExamples, Algorithm, Options, Nodes)->
	{CheckOptions, AlgorithmOptions} = split_learn_options(Options),
	case check_learn_args(Attributes, Class, TrainingExamples, CheckOptions) of
		%ok -> Algorithm:learn(Attributes, Class, TrainingExamples, AlgorithmOptions);
		ok ->
			spawn_link(?MODULE, supervisor_spawner, [self(), Algorithm, learn, [Attributes, Class, TrainingExamples, AlgorithmOptions], Nodes, no_ets]),
			receive
				{ok, stopped} -> {ok, stopped};
				{ok, Result} -> Result;
				{error, Reason} -> {error, Reason}
			end;
		{error, _Reason} = Error -> Error
	end;
learn_naive(Attributes, ClassName, TrainingExamples, Algorithm, Options, Nodes) when is_atom(ClassName) or is_integer(ClassName) ->
	{CheckOptions, AlgorithmOptions} = split_learn_options(Options),

	case choose_class(Attributes, TrainingExamples, ClassName) of
		{NewAttributes, Class, NewTrainingExamples} ->
			{CheckOptions, AlgorithmOptions} = split_learn_options(Options),
			case check_learn_args(NewAttributes, Class, NewTrainingExamples, CheckOptions) of
				%ok -> Algorithm:learn(NewAttributes, Class, NewTrainingExamples, AlgorithmOptions);
				ok -> 
					spawn_link(?MODULE, supervisor_spawner, [self(), Algorithm, learn, [NewAttributes, Class, NewTrainingExamples, AlgorithmOptions], Nodes, no_ets]),
					receive
						{ok, stopped} -> {ok, stopped};
						{ok, Result} -> Result;
						{error, Reason} -> {error, Reason}
					end;
				{error, _Reason} = Error -> Error
			end;
		Error -> Error
	end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%








-spec learn(Attributes :: [attribute()], Class :: class() | name(), TrainingExamples :: [training_example()], Algorithm :: atom(), Options :: [Option :: any()]) -> {ok, Classifier :: classifier()} | {error, Reason :: any()}.
% @spec learn(Attributes, Class, TrainingExamples, Algorithm, Options) -> {ok, Classifier} | {error, Reason} where Attributes = [attribute()], Class = class() | name(), TrainingExamples = [training_example()], Algorithm = atom(), Options = [Option], Option = any(), Classifier = classifier(), Reason = any()
% @doc Creates a classifier for a Class using Algorithm. TrainingExamples should as desribed in Attributes and be classified to class from Class. Algorithm should be an atom() specifying the proper module. Options are passed to an Algorithm and are specified by the Algorithm documentation. Returned Classifier is closely related to the Algorithm. <br/>Algorithm module should export ``learn(Attributes, Class, TrainingExamples, Options)''. <br/> You can pass the additional options for both arguments correctness checking and for the learning algorithm itself. Note that unrecognized options will be ignored. <br/> The arguments correctness checking Options are: <ul><li>``no_check'' - there will be no arguments correctness checkings</li><li>``attributes_ok'' - there will be no Attributes correctness checking</li><li>``class_ok'' - there will be no Class correctness checking</li><li>``trainingexamples_ok'' - there will be no TrainingExamples correctness checking</li></ul><br/> <i>author:</i> beben, kumateusz
learn(Attributes, #class{} = Class, TrainingExamples, Algorithm, Options)->
	learn(Attributes, Class, TrainingExamples, Algorithm, Options, [node()]);
learn(Attributes, ClassName, TrainingExamples, Algorithm, Options) when is_atom(ClassName) or is_integer(ClassName) ->
	learn(Attributes, ClassName, TrainingExamples, Algorithm, Options, [node()]).

-spec learn(Attributes :: [attribute()], Class :: class() | name(), TrainingExamples :: [training_example()], Algorithm :: atom(), Options :: [Option :: any()], Nodes :: [node()]) -> {ok, Classifier :: classifier()} | {error, Reason :: any()}.
% @spec learn(Attributes, Class, TrainingExamples, Algorithm, Options, Nodes) -> {ok, Classifier} | {error, Reason} where Attributes = [attribute()], Class = class() | name(), TrainingExamples = [training_example()], Algorithm = atom(), Options = [Option], Option = any(), Nodes = [node()], Classifier = classifier(), Reason = any()
% @doc Creates a classifier for a Class using Algorithm. TrainingExamples should as desribed in Attributes and be classified to class from Class. Algorithm should be an atom() specifying the proper module. Options are passed to an Algorithm and are specified by the Algorithm documentation. Nodes array consists of nodes that are to be used in computation. Returned Classifier is closely related to the Algorithm. <br/>Algorithm module should export ``learn(Attributes, Class, TrainingExamples, Options)''. <br/> You can pass the additional options for both arguments correctness checking and for the learning algorithm itself. Note that unrecognized options will be ignored. <br/> The arguments correctness checking Options are: <ul><li>``no_check'' - there will be no arguments correctness checkings</li><li>``attributes_ok'' - there will be no Attributes correctness checking</li><li>``class_ok'' - there will be no Class correctness checking</li><li>``trainingexamples_ok'' - there will be no TrainingExamples correctness checking</li></ul><br/> <i>author:</i> beben, kumateusz
learn(Attributes, #class{} = Class, TrainingExamples, Algorithm, Options, Nodes)->
	{CheckOptions, AlgorithmOptions} = split_learn_options(Options),
	case check_learn_args(Attributes, Class, TrainingExamples, CheckOptions) of
		%ok -> Algorithm:learn(Attributes, Class, TrainingExamples, AlgorithmOptions);
		ok ->
			spawn_link(?MODULE, supervisor_spawner, [self(), Algorithm, learn, [Attributes, Class, TrainingExamples, AlgorithmOptions], Nodes]),
			receive
				{ok, stopped} -> {ok, stopped};
				{ok, Result} -> Result;
				{error, Reason} -> {error, Reason}
			end;
		{error, _Reason} = Error -> Error
	end;
learn(Attributes, ClassName, TrainingExamples, Algorithm, Options, Nodes) when is_atom(ClassName) or is_integer(ClassName) ->
	{CheckOptions, AlgorithmOptions} = split_learn_options(Options),

	case  choose_class(Attributes, TrainingExamples, ClassName) of
		{NewAttributes, Class, NewTrainingExamples} ->
			{CheckOptions, AlgorithmOptions} = split_learn_options(Options),
			case check_learn_args(NewAttributes, Class, NewTrainingExamples, CheckOptions) of
				%ok -> Algorithm:learn(NewAttributes, Class, NewTrainingExamples, AlgorithmOptions);
				ok -> 
					spawn_link(?MODULE, supervisor_spawner, [self(), Algorithm, learn, [NewAttributes, Class, NewTrainingExamples, AlgorithmOptions], Nodes]),
					receive
						{ok, stopped} -> {ok, stopped};
						{ok, Result} -> Result;
						{error, Reason} -> {error, Reason}
					end;
				{error, _Reason} = Error -> Error
			end;
		Error -> Error
	end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec mine(Data :: [ [any()] ], Module :: atom(), Options :: [Option :: any()], Nodes :: [node()]) -> {ok, Result :: any()} | {error, Reason :: any()}.
% @spec mine(Data, Module, Options, Nodes) -> {ok, Result} | {error, Reason} where Data = [ [any()] ], Module = atom(), Options = [any()], Nodes = [node()], Result = any(), Reason = any()
% @doc Data Mining in the given data. Implemented modules are : apriori <br/> <i>author:</i> beben, kumateusz
mine(Data, Algorithm, Options, Nodes) ->
	spawn_link(?MODULE, supervisor_spawner, [self(), Algorithm, mine, [Data, Options], Nodes]),
	receive
		{ok, stopped} -> {ok, stopped};
		{ok, Result} -> {ok, Result};
		{error, Reason} -> {error, Reason}
	end.
%%%%%%%%%%%%%%%
-spec mine(Data :: [ [any()] ], Module :: atom(), Options :: [Option :: any()]) -> {ok, Result :: any()} | {error, Reason :: any()}.
% @spec mine(Data, Module, Options) -> {ok, Result} | {error, Reason} where Data = [ [any()] ], Module = atom(), Options = [any()], Result = any(), Reason = any()
% @doc Data Mining in the given data. Implemented modules are : apriori <br/> <i>author:</i> beben, kumateusz
mine(Data, Algorithm, Options) ->
	mine(Data, Algorithm, Options, [node()]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec classify(Classifier :: classifier(), Example :: tuple(), Options :: [Option :: any()]) -> {ok, Category :: name()} | {error, Reason :: any()}.
% @spec classify(Classifier, Example, Options) -> {ok, Category} | {error, Reason} where Classifier = classifier(), Algorithm = atom(), Example = example(), Category = category(), Reason = any()
% @doc Clasifies given Example of Attributes using Algorithm related with Classifier. Algorithm ahould be an atom() specifying the proper module. Classifier has to match the Algorithm. Example should contain values described in Attributes. <br/>Algorithm module should export ``classify(Attributes, Classifier, Example, Options)''. <br/> <i>author:</i> beben
classify(#classifier{ algorithm = Algorithm } = Classifier, Example, Options) ->
	Algorithm:classify(Classifier, Example, Options).

-spec transform_attributes(Type :: atom(), Attributes :: [attribute()], Class :: class(), TrainingExamples :: [training_example()], Options :: [Option :: any()]) -> {NewAttributes :: [attribute()], Transformer :: transformer()} | {error, Reason :: any()}.
% @spec transform_attributes(Type, Attributes, Class, TrainingExamples, Options) -> {NewAttributes, Transformer} | {error, Reason} where Type = atom(), Attributes = [attribute()], Class = class(), TrainingExamples = [training_example()], Options = [any()], NewAttributes = [attribute()], Transformer = transformer, Reason = any()
% @doc Returns new definition of attributes NewAttributes and Transformer- the structure that is used to transform (training) examples. The possible Type values are : * discrete.
transform_attributes(Type, Attributes, Class, TrainingExamples, Options) ->
	transform:transform_attributes(Type, Attributes, Class, TrainingExamples, Options).

-spec transform_example(Transformer :: transformer(), Example :: training_example() | example()) -> TransformedExample :: training_example() | example() | {error, Reason :: any()}.
% @spec transform_example(Transformer, Example) -> TransformedExample | {error, Reason} where Transformer = transformer(), Example = training_example() | example(), TransformedExample = training_example() | example(), Reason = any()
% @doc Returns the example(or Training Example) transformed into new definition.
transform_example(Transformer, Example) ->
	transform:transform_example(Transformer, Example).


-spec read_attributes(Filename :: [any()], Format :: xml|arff, Options :: [any()]) -> {ok, Attributes :: [attribute()]} | {error, Reason :: atom()}.
% @spec read_attributes(Filename, Format, Options) -> {ok, Attributes} | {error, Reason} where Filename = [any()], Attributes = [attribute()], Format = xml|arff, Options = [any()], Reason = atom()
% @doc Parses XML file containing attributes definition and returns ready to use Attributes. <br /> <i>author:</i> matis 
read_attributes(_Filename, xml, [_|_]) ->
	{error, bad_options};
read_attributes(Filename, xml, []) ->
	case xmerl_scan:file(Filename, [{space, normalize}]) of
		{error, Reason} -> io:format("Could not parse file \"~s\"~n", [Filename]), {error, Reason};
		{ParsedXml, _Misc} -> parse_attributes_xml(ParsedXml)
	end;
read_attributes(Filename, arff, []) ->
	case arff:file(Filename) of
		{error, Reason} -> {error, Reason};
		[ParsedResult] -> parse_attributes_arff(ParsedResult)
	end;
read_attributes(_Filename, arff, _) ->
	{error, bad_options};
read_attributes(Filename, c45, []) ->
	case c45_format:file(Filename) of
		{error, Reason} -> {error, Reason};
		ParsedResult -> parse_attributes_c45(ParsedResult)
	end;
read_attributes(_Filename, c45, _) ->
	{error, bad_options}.

-spec read_classes(Filename :: [any()], Format :: xml|arff, Options :: [any()]) -> {ok, Classes :: [class()]} | {error, Reason :: atom()}.
% @spec read_classes(Filename, Format, Options) -> {ok, Classes} | {error, Reason} where Filename = [any()], Classes = [class()], Attributes = [attribute()], Format = xml|arff, Options = [any()], Reason = atom()
% @doc Parses XML file containing classes definition and returns a list of classes. <br /> <i>author:</i> matis 
% @deprecated Use {@link read/3} instead.
read_classes(Filename, xml, _Options) ->
	case xmerl_scan:file(Filename, [{space, normalize}]) of
		{error, Reason} -> io:format("Could not parse file \"~s\"~n", [Filename]), {error, Reason};
		{ParsedXml, _Misc} -> parse_classes(ParsedXml)
	end;
read_classes(Filename, arff, [{class, ClassName}|_T]) ->
	case arff:file(Filename) of
		{error, Reason} -> {error, Reason};
		[ParsedResult] -> parse_classes_arff(ParsedResult, ClassName)
	end;
read_classes(_Filename, arff, [_|_T]) ->
	{error, bad_options};
read_classes(_Filename, arff, []) ->
	{error, bad_options}.



-spec read(Format :: xml|arff, Options :: [any()]) -> {Attributes :: [attribute()], TrainingExamples :: example()} | {error, Reason :: atom()}.
% @spec read(Format, Options) -> {Attributes, TrainingExamples} | {error, Reason :: atom()} where Format = xml|arff, Options = [any()], Attributes = [attribute()], TrainingExamples = [example()], Reason = atom()
% @doc Read attributes and training examples without specifying class. If chosen format is xml or c45, then two  options are required: <br /> - ``{attributes, Filename}'' - File containing attributes <br /> - ``{tes, Filename}'' - File containing training examples <br /> ARFF requires one option: <br /> - ``{file, Filename}'' - File containing both attributes and data
read(xml, Options) ->
	read(xml, Options, {undefined, undefined}, none);
read(arff, Options) ->
	read(arff, Options, {undefined}, none);
read(c45, Options) ->
	read(c45, Options, {undefined, undefined}, nil).

-spec read(Format :: xml|arff, Classname :: atom(), Options :: [any()]) -> {Attributes :: [attribute()], Class :: class(), TrainingExamples :: example()} | {error, Reason :: atom()}.
% @spec read(Format, Classname, Options) -> {Attributes, Class, TrainingExamples} | {error, Reason :: atom()} where Format = xml|arff, Classname = atom(), Options = [any()], Attributes = [attribute()], Class = class(), TrainingExamples = [example()], Reason = atom()
% @doc Read attributes and training examples with specified class. If chosen format is xml or c45, then two  options are required: <br /> - ``{attributes, Filename}'' - File containing attributes <br /> - ``{tes, Filename}'' - File containing training examples <br /> ARFF requires one option: <br /> - ``{file, Filename}'' - File containing both attributes and data <br /> If chosen format is c45, then Classname is ignored, as class is specified within attributes file. 
read(xml, Classname, Options) ->
	read(xml, Options, {undefined, undefined}, Classname);
read(arff, Classname, Options) ->
	read(arff, Options, {undefined}, Classname);
read(c45, _Classname, Options) ->
	read(c45, Options, {undefined, undefined}, nil).

read(xml, [{attributes, NewFilename}|T], {_AttrFilename, DataFilename}, Classname) ->
	read(xml, T, {NewFilename, DataFilename}, Classname);
read(xml, [{tes, NewFilename}|T], {AttrFilename, _DataFilename}, Classname) ->
	read(xml, T, {AttrFilename, NewFilename}, Classname);
read(xml, [], OptionsTuple, Classname) ->
	read_everything(xml, OptionsTuple, Classname);
read(arff, [{file, Filename}|T], {_}, Classname) ->
	read(arff, T, {Filename}, Classname);
read(arff, [], OptionsTuple, Classname) ->
	read_everything(arff, OptionsTuple, Classname);
read(c45, [{attributes, NewFilename}|T], {_AttrFilename, DataFilename}, _) ->
	read(c45, T, {NewFilename, DataFilename}, nil);
read(c45, [{tes, NewFilename}|T], {AttrFilename, _DataFilename}, _) ->
	read(c45, T, {AttrFilename, NewFilename}, nil);
read(c45, [], OptionsTuple, _) ->
	read_everything(c45, OptionsTuple, nil).


%%%%%%
% @doc Loads data for Data Mining from CSV file.
read_mine_data(Filename) ->
	Lines = data:file(Filename),
	data_mine_to_list_acu(Lines, []).

data_mine_to_list_acu([] = _Lines, Acu) ->
	{ok, lists:reverse(Acu)};
data_mine_to_list_acu([{normal, Tuple} | Lines], Acu) ->
	data_mine_to_list_acu(Lines, [tuple_to_list(Tuple)|Acu]);
data_mine_to_list_acu([Error | Lines], Acu) ->
	error.


-spec read_examples(Filename :: string()) -> {ok, Examples :: [example()]} | {error, Reason :: any()}.
% @spec read_examples(Filename) -> {ok, Examples} | {error, Reason} where Filename = string(), Examples = [example()], Reason = any()
% @doc Reads Examples from CSV Filename. The function is unaware of Attributes definitions. <br/> <i>author:</i> beben
read_examples(_Filename) ->
	{error, not_implemented}.

-spec read_trainingexamples(Filename :: string()) -> {ok, TrainingExamples :: [training_example()]} | {error, Reason :: any()}.
% @spec read_trainingexamples(Filename) -> {ok, TrainingExamples} | {error, Reason} where Filename = string(), TrainingExamples = [training_example()], Reason = any()
% @doc Reads TrainingExamples from CSV Filename. The function is unaware of Attributes or Class definitions. <br/> <i>author:</i> beben
read_trainingexamples(_Filename) ->
	{error, not_implemented}.		


-spec write_classifier(Filename :: string(), Classifier :: classifier()) -> ok | {error, Reason :: any()}.
% @spec write_classifier(Filename, Classifier) -> ok | {error, Reason} where Filename = string(), Classifier = classifier(), Reason = any()
% @doc Writes classifier to Filename. Classifier is human readable. <br/> <i>author:</i> beben
write_classifier(Filename, Classifier) ->
	case file:open(Filename, [write]) of
		{error, Reason} -> {error, Reason};
		{ok, IoDevice} -> io:fwrite(IoDevice, "~p", [Classifier]), file:close(IoDevice), ok
	end.		

-spec read_classifier(Filename :: string()) -> {ok, Classifier :: classifier()} | {error, Reason :: any()}.
% @spec read_classifier(Filename :: string()) -> {ok, Classifier} | {error, Reason} where Classifier = classifier(), Reason = any()
% @doc Reads classifier from Filename. Classifier is human readable. <br/> <i>author:</i> beben
read_classifier(Filename) ->
	{ok, B} = file:read_file(Filename),
	{ok, ItemTokens, _EndLocation} = erl_scan:string(binary_to_list(B) ++ "."), 
	erl_parse:parse_term(ItemTokens).

-spec choose_class(Attributes :: [attribute()], TrainingExamples :: [example()], ClassName :: name()) -> {NewAttributes :: [attribute()], Class :: class(), NewTrainingExamples :: [training_example()]} | {error, Reason :: any()}.
% @spec choose_class(Attributes, TrainingExamples, ClassName) -> {NewAttributes, Class, NewTrainingExamples} | {error, Reason} where Attributes = [attribute()], TrainingExamples = [example()], ClassName = name(), NewAttributes = [attribute()], Class = class(), NewTrainingExamples = [training_example()], Reason = any()
% @doc Removes the Attribute with name ClassName from the Attributes, reinterprets this Attribute as Class and changes TrainingExamples allowing them to be used in the Class learning. Returns the NewAttributes and NewTrainingExamples that can be used during learning process. Attribute chosen as a Class has to be of nominal or ordered type.
choose_class(Attributes, TrainingExamples, ClassName) ->
	case mllib_tools:get_attribute_pos(Attributes, ClassName) of
		error -> {error, no_class};
		Number -> 	
			Pos = Number,
			Attribute = mllib_tools:get_attribute(Attributes, ClassName),

			case Attribute#attribute.type of
			  continuous -> {error, not_supported};
			  _Supported ->	

				{NewAttributes, FutureClass} = mllib_tools:exclude(Attributes, Pos),
				Class = #class{ name = FutureClass#attribute.name, categories = FutureClass#attribute.values },

				NewTrainingExamples = [ fetch_class(tuple_to_list(TE), Pos) || TE <- TrainingExamples ],

				{NewAttributes, Class, NewTrainingExamples}
			end
	end.


%%%
%%% Local functions
%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Learn options
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% pass unrecognized options to the algorithm
split_learn_options(Options) ->
	split_learn_options(Options, [], []).
	
split_learn_options([], CheckOptions, AlgorithmOptions) ->
	{CheckOptions, AlgorithmOptions};
split_learn_options([Option | Options], CheckOptions, AlgorithmOptions) ->
	case lists:member(Option, ?RECOGNIZED_LEARN_OPTIONS) of
		true -> split_learn_options(Options, [Option|CheckOptions], AlgorithmOptions);
		false -> split_learn_options(Options, CheckOptions, [Option|AlgorithmOptions])
	end.

% remove or add proper options if not in defaults
complete_check_funs([], Funs) ->
	?LOG("Checkers funs: ~p~n", [Funs]),
	Funs;
complete_check_funs([no_check|Options], _Funs) ->
	?LOG("Removing all check funs~n", []),
	complete_check_funs(Options, []);
complete_check_funs([attributes_ok|Options], Funs) ->
	?LOG("Removing Attributes check fun~n", []),
	complete_check_funs(Options, lists:keydelete(check_attributes_correct, 1, Funs));
complete_check_funs([class_ok|Options], Funs) ->
	?LOG("Removing Class check fun~n", []),
	complete_check_funs(Options, lists:keydelete(check_class_correct, 1, Funs));
complete_check_funs([trainingexamples_ok|Options], Funs) ->
	?LOG("Removing TrainingExamples check fun~n", []),
	complete_check_funs(Options, lists:keydelete(check_trainingexamples_correct, 1, Funs)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Functions checking input data correctness
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

check_learn_args(Attributes, Class, TrainingExamples, Options) ->
	DefaultCheckFuns = [{check_attributes_correct, fun check_attributes_correct/3}, 
				{check_class_correct, fun check_class_correct/3}, 
				{check_trainingexamples_correct, fun check_trainingexamples_correct/3}],
	CheckFuns = complete_check_funs(Options, DefaultCheckFuns),
	fire_check_funs(CheckFuns, Attributes, Class, TrainingExamples).

fire_check_funs([], _Attributes, _Class, _TrainingExamples) ->
	ok;
fire_check_funs([{_Key, Check} | CheckFuns], Attributes, Class, TrainingExamples) ->
	case Check(Attributes, Class, TrainingExamples) of
		ok -> fire_check_funs(CheckFuns, Attributes, Class, TrainingExamples);
		{error, _Reason} = Error -> Error
	end.



% check if Attributes are correct 
check_attributes_correct([] = _Attributes, _Class, _TrainingExamples) ->
	{error, "Attributes are empty"};
check_attributes_correct(Attributes, _Class, _TrainingExamples) ->
	?DETAIL("In check_attributes_correct/3: Attributes: ~p~n", [Attributes]),
	attributes_correct(Attributes).


%%%%%%
% TODO: TEMPORARY
%%%%%%
%attributes_correct(_) ->
%	ok;

attributes_correct([]) ->
	ok;
attributes_correct([#attribute{name = undefined}|_Attributes]) ->
	{error, "Attribute name undefined"};
attributes_correct([#attribute{type = undefined}|_Attributes]) ->
	{error, "Attribute type undefined"};
attributes_correct([#attribute{values = []}|_Attributes]) ->
	{error, "Attribute values empty"};
attributes_correct([#attribute{type = continuous, values = Values}|_Attributes]) when not (is_tuple(Values) and (size(Values) == 2))->
	{error, "Bad continuous values format"};
attributes_correct([#attribute{name = Name, type = Type, values = Values}|Attributes]) ->
	{LeftBound, RightBound} = case Type of 
					continuous -> Values;
					_Other      ->  {0,0}
					end,
	case	(
		(is_atom(Name) orelse is_integer(Name)) andalso (
		((Type == nominal orelse Type == ordered) andalso (length([ X || X <- Values, is_atom(X) orelse is_integer(X)])==length(Values))) orelse
		( (Type == continuous) andalso is_number(LeftBound) andalso is_number(RightBound) andalso (LeftBound =< RightBound) ) orelse
		( (Type == continuous) andalso (min==LeftBound) andalso is_number(RightBound) ) orelse
		( (Type == continuous) andalso is_number(LeftBound) andalso (max==RightBound) ) orelse
		( (Type == continuous) andalso (min==LeftBound) andalso (max==RightBound) )
		)) of true -> attributes_correct(Attributes);
		_ -> {error, "Bad #attribute record member value"}
	end;
attributes_correct([_Error|_Attributes]) ->
	{error, "Attributes member is not a #attribute record"}.

% check if a list of Classes is correct
check_classes_correct([H|T]) ->
	case check_class_correct(null, H, null) of
		ok -> check_classes_correct(T);
		Error -> Error
	end;
check_classes_correct([]) ->
	ok.

% check if Class is correct
check_class_correct(_Attributes, #class{name = undefined}, _TrainingExamples) ->
	{error, "Class name undefined"};
check_class_correct(_Attributes, #class{categories = []} = _Class, _TrainingExamples) ->
	{error, "Class categories empty"};
check_class_correct(_Attributes, #class{name = Name, description = Description, categories = Categories} = _Class, _TrainingExamples) ->
	case	(
		(is_atom(Name) orelse is_integer(Name)) and 
		(is_list(Description)) and
		( length([ X || X <- Categories, is_atom(X) orelse is_integer(X)])==length(Categories) )
		) of true -> ok;
		_Else -> {error, "Bad #class record member value"}
	end;
check_class_correct(_Attributes, _Error, _TrainingExamples) ->
	{error, "Class is not a #class record"}.

% check if TrainingExamples are correct 
check_trainingexamples_correct(_Attributes, _Class, [] = _TrainingExamples) ->
	{error, "TrainingExamples are empty"};
check_trainingexamples_correct(Attributes, _Class, TrainingExamples) ->
	trainingexamples_correct(TrainingExamples, length(Attributes)).

%%%%%%%%%
% TODO: TEMPORARY
%%%%%%%%%
%trainingexamples_correct(_, _) ->
%	ok;


trainingexamples_correct([], _AttributesLength) ->
	ok;
trainingexamples_correct([{DataAtribs, _Class}|TrainingExamples], AttributesLength) when is_tuple(DataAtribs), size(DataAtribs)==AttributesLength ->
	trainingexamples_correct(TrainingExamples, AttributesLength);
trainingexamples_correct([_Error|_TrainingExamples], _AttributesLength) ->
	{error, "TrainingExamples member is incorrect"}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Configuration files parsing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% parse given xml string and return attributes
% new values for duplicate attributes (name, type) as well as bounds replace the old values
parse_attributes_xml(#xmlElement{name = 'Attributes', content = Content}) ->
	AttributesResult = attribute_list(Content, []),
	case AttributesResult of
		{ok, Attributes} ->
			case attributes_correct(Attributes) of
				{error, _Reason} -> {error, "Attributes are incorrect"};
				ok -> AttributesResult
			end;
		{error, Reason} -> {error, Reason}
	end;
parse_attributes_xml(_) ->
	{error, parse_error}.

attribute_list([#xmlText{}|T], Acc) ->
	attribute_list(T, Acc);
attribute_list([Attribute = #xmlElement{name = 'Attribute'}|T], Acc) ->
	case attribute(Attribute) of
		{ok, Ret} -> attribute_list(T, [Ret|Acc]);
		{error, Reason} -> {error, Reason}
	end;
attribute_list([], Acc) ->
	{ok, lists:reverse(Acc)};
attribute_list(_, _) ->
	{error, parse_error}.

attribute(#xmlElement{content = Content, attributes = Attributes}) ->
	case append_values(append_attributes_attr(null, Attributes), Content) of
		{error, Reason} -> {error, Reason};
		Ret -> {ok, Ret}
	end.

append_attributes_attr(null, Attributes) ->
	append_attributes_attr(#attribute{}, Attributes);
append_attributes_attr(#attribute{name = _Name, type = Type, values = Values}, [#xmlAttribute{name = 'name', value = NewName}|T]) ->
	append_attributes_attr(#attribute{name = list_to_atom(string:strip(NewName)), type = Type, values = Values}, T);
append_attributes_attr(#attribute{name = Name, type = _Type, values = Values}, [#xmlAttribute{name = 'type', value = NewType}|T]) ->
	append_attributes_attr(#attribute{name = Name, type = list_to_atom(string:strip(NewType)), values = Values}, T);
append_attributes_attr(#attribute{name = Name, type = Type, values = Values}, [#xmlAttribute{}|T]) -> %ignore unrecognized attributes
	append_attributes_attr(#attribute{name = Name, type = Type, values = Values}, T);
append_attributes_attr(Ret = #attribute{}, []) ->
	Ret.

append_values(null, Content) ->
	append_values(#attribute{values = []}, Content);
append_values(#attribute{name = Name, type = Type, values = undefined}, Content) ->
	append_values(#attribute{name = Name, type = Type, values = []}, Content);
append_values(Attribute, [#xmlText{}|T]) ->
	append_values(Attribute, T);
append_values(#attribute{name = Name, type = continuous, values = []}, Content) ->
	append_values(#attribute{name = Name, type = continuous, values = [max, min]}, Content);
append_values(#attribute{name = Name, type = continuous, values = [VH|_VT]}, [#xmlElement{name = 'LeftBound', content = NewLeftBound}|T]) ->
	case get_num_from_content(NewLeftBound) of
		{error, Reason} -> {error, Reason};
		Num -> append_values(#attribute{name = Name, type = continuous, values = [VH|[Num]] }, T)
	end;
append_values(#attribute{name = Name, type = continuous, values = [_VH|VT]}, [#xmlElement{name = 'RightBound', content = NewRightBound}|T]) ->
	case get_num_from_content(NewRightBound) of
		{error, Reason} -> {error, Reason};
		Num -> append_values(#attribute{name = Name, type = continuous, values = [Num|VT] }, T)
	end;
append_values(#attribute{name = Name, type = Type, values = Values}, [#xmlElement{name = 'Value', content = NewValue}|T]) ->
	append_values(#attribute{name = Name, type = Type, values = [get_val_from_content(NewValue)|Values]}, T);
append_values(_Attribute, [#xmlElement{}|_T]) ->
	{error, "Incorrect xml file."};
append_values(#attribute{name = Name, type = continuous, values = Values}, []) ->
	#attribute{name = Name, type = continuous, values = list_to_tuple(lists:reverse(Values))};
append_values(#attribute{name = Name, type = Type, values = Values}, []) ->
	#attribute{name = Name, type = Type, values = lists:reverse(Values)}.

% parse given xml string and return classes
parse_classes(#xmlElement{name = 'Classes', content = Content}) ->
	ClassesResult = class_list(Content, []),
	case ClassesResult of
		{ok, Classes} ->
			case check_classes_correct(Classes) of
				{error, _Reason} -> {error, parse_error};
				ok -> ClassesResult
			end;
		_Error -> {error, parse_error}
	end;
parse_classes(_) ->
	{error, parse_error}.

class_list([#xmlText{}|T], Acc) ->
	class_list(T, Acc);
class_list([Class = #xmlElement{name = 'Class'}|T], Acc) ->
	case class(Class) of
		{ok, Ret} -> class_list(T, [Ret|Acc]);
		{error, _} -> {error, parse_error}
	end;
class_list([], Acc) ->
	{ok, lists:reverse(Acc)};
class_list(_, _) ->
	{error, parse_error}.

class(#xmlElement{content = Content, attributes = Attributes}) ->
	case append_categories(append_attributes_class(null, Attributes), Content) of
		{error, _} -> {error, parse_error};
		Ret -> {ok, Ret}
	end.

append_attributes_class(null, Attributes) ->
	append_attributes_class(#class{}, Attributes);
append_attributes_class(#class{name = _Name, description = Description, categories = Categories}, [#xmlAttribute{name = 'name', value = NewName}|T]) ->
	append_attributes_class(#class{name = list_to_atom(string:strip(NewName)), description = Description, categories = Categories}, T);
append_attributes_class(#class{name = Name, description = _Description, categories = Categories}, [#xmlAttribute{name = 'description', value = NewDescription}|T]) ->
	append_attributes_class(#class{name = Name, description = NewDescription, categories = Categories}, T);
append_attributes_class(#class{name = Name, description = Description, categories = Categories}, [#xmlAttribute{}|T]) -> %ignore unrecognized attributes
	append_attributes_class(#class{name = Name, description = Description, categories = Categories}, T);
append_attributes_class(Class, []) ->
	Class.

append_categories(null, Content) ->
	append_categories(#class{}, Content);
append_categories(Class, [#xmlText{}|T]) ->
	append_categories(Class, T);
append_categories(#class{name = Name, description = Description, categories = Categories}, [#xmlElement{name = 'Category', content = Content}|T]) ->
	append_categories(#class{name = Name, description = Description, categories = [get_val_from_content(Content)|Categories]}, T);
append_categories(_Class, [#xmlElement{}|_T]) ->
	{error, parse_error};
append_categories(#class{name = Name, description = Description, categories = Categories}, []) ->
	#class{name = Name, description = Description, categories = lists:reverse(Categories)}.

%extract useful data from XML content
get_num_from_content([#xmlText{value = Val}]) ->
	case string:to_float(string:strip(Val)) of
		{error, _} ->
			case string:to_integer(string:strip(Val)) of
				{error, _} -> {error, not_a_number};
				{Int, _Rem} -> Int
			end;
		{Float, _Rem} -> Float
	end.

get_val_from_content(Arg = [#xmlText{value = Val}]) ->
	case get_num_from_content(Arg) of
		{error, _} -> list_to_atom(string:strip(Val));
		Num -> Num
	end.

% parse attributes in arff format
parse_attributes_arff(ParsedAttributes) ->
	parse_attributes_arff(ParsedAttributes, []).

parse_attributes_arff([{attribute, {Name, numeric}}|T], Acc) ->
	parse_attributes_arff(T, [#attribute{name = Name, type = continuous, values = {min, max}}|Acc]);
parse_attributes_arff([{attribute, {Name, integer}}|T], Acc) ->
	parse_attributes_arff(T, [#attribute{name = Name, type = continuous, values = {min, max}}|Acc]);
parse_attributes_arff([{attribute, {Name, real}}|T], Acc) ->
	parse_attributes_arff(T, [#attribute{name = Name, type = continuous, values = {min, max}}|Acc]);
parse_attributes_arff([{attribute, {_Name, date}}|_T], _Acc) ->
	{error, date_not_supported};
parse_attributes_arff([{attribute, {_Name, string}}|_T], _Acc) ->
	{error, string_not_supported};
parse_attributes_arff([{attribute, {Name, [NH|NT]}}|T], Acc) ->
	parse_attributes_arff(T, [#attribute{name = Name, type = nominal, values = [NH|NT]}|Acc]);
parse_attributes_arff([{attribute, {_Name, _}}|_T], _Acc) ->
	{error, parse_error};
parse_attributes_arff([], Acc) ->
	{ok, lists:reverse(Acc)};
parse_attributes_arff(_, _Acc) ->
	{error, parse_error}.

% parse class in arff format
parse_classes_arff([{attribute, {ClassName, [NH|NT]}}|_T], ClassName) ->
	{ok, [#class{name = ClassName, categories = [NH|NT]}]};
parse_classes_arff([{attribute, {ClassName, _}}|_T], ClassName) ->
	{error, parse_error};
parse_classes_arff([{attribute, {_Name, _}}|T], ClassName) ->
	parse_classes_arff(T, ClassName);
parse_classes_arff([], _ClassName) ->
	{error, parse_error}.


% parse attributes in c45 format
parse_attributes_c45(ParsedAttributes) ->
	parse_attributes_c45(ParsedAttributes, []).

parse_attributes_c45([{attribute, {_, ignored}}|T], Acc) ->
	parse_attributes_c45(T, Acc);
parse_attributes_c45([{class, _}|T], Acc) ->
	parse_attributes_c45(T, Acc);
parse_attributes_c45([{attribute, {Name, continuous}}|T], Acc) ->
	parse_attributes_c45(T, [#attribute{name = Name, type = continuous, values = {min, max}}|Acc]);
parse_attributes_c45([{attribute, {Name, [VH|VT]}}|T], Acc) ->
	parse_attributes_c45(T, [#attribute{name = Name, type = nominal, values = [VH|VT]}|Acc]);
parse_attributes_c45([], Acc) ->
	{ok, lists:reverse(Acc)};
parse_attributes_c45(_, _Acc) ->
	{error, parse_error}.

% parse class in c45 format
parse_class_c45([{attribute, _}|T]) ->
	parse_class_c45(T);
parse_class_c45([{class, Values}|_]) ->
	{ok, #class{name = class, description = [], categories = Values}};
parse_class_c45([]) ->
	{error, no_class_found}.

% get positions of ignored attributes
ignored_attributes_positions_c45(ParsedAttributes) ->
	ignored_attributes_positions_c45(ParsedAttributes, 1, []).

ignored_attributes_positions_c45([{attribute, {_, ignored}}|T], Pos, Acc) ->
	ignored_attributes_positions_c45(T, Pos + 1, [Pos|Acc]);
ignored_attributes_positions_c45([_|T], Pos, Acc) ->
	ignored_attributes_positions_c45(T, Pos + 1, Acc);
ignored_attributes_positions_c45([], _, Acc) ->
	{ok, Acc}.

% remove ignored attributes values from the training data
remove_ignored_c45(Tes, []) ->
	{ok, Tes};
remove_ignored_c45(Tes, IgnoredPositions) ->
	remove_ignored_c45(Tes, IgnoredPositions, []).

remove_ignored_c45([H|T], IgnoredPositions, Acc) ->
	remove_ignored_c45(T, IgnoredPositions, [list_to_tuple(mllib_tools:exclude_multi(tuple_to_list(H), IgnoredPositions))|Acc]);
remove_ignored_c45([], _, Acc) ->
	{ok, lists:reverse(Acc)}.
	

%%%%%%%%%%%%%

read_everything(xml, {_, undefined}, _) ->
	{error, no_tes_filename_given};
read_everything(xml, {undefined, _}, _) ->
	{error, no_attributes_filename_given};
read_everything(xml, {AttributesFilename, DataFilename}, Classname) ->
	{ok, Attributes} = read_attributes(AttributesFilename, xml, []),
	{ok, Tes} = read_tes(DataFilename, xml, length(Attributes)),
	case Classname of
		none -> {Attributes, Tes};
		_ -> choose_class(Attributes, Tes, Classname)
	end;
read_everything(arff, {undefined}, _) ->
	{error, no_filename_given};
read_everything(arff, {Filename}, Classname) ->
	{ok, Attributes} = read_attributes(Filename, arff, []),
	{ok, Tes} = read_tes(Filename, arff, length(Attributes)),
	case Classname of
		none -> {Attributes, Tes};
		_ -> choose_class(Attributes, Tes, Classname)
	end;
read_everything(c45, {_, undefined}, _) ->
	{error, no_tes_filename_given};
read_everything(c45, {undefined, _}, _) ->
	{error, no_attributes_filename_given};
read_everything(c45, {AttributesFilename, DataFilename}, _) ->
	{ok, Attributes} = read_attributes(AttributesFilename, c45, []),
	ParsedAttributes = c45_format:file(AttributesFilename),
	{ok, Class} = parse_class_c45(ParsedAttributes),
	{ok, IgnoredAttributesPositions} = ignored_attributes_positions_c45(ParsedAttributes),
	{ok, Tes} = read_tes(DataFilename, c45, length(Attributes)),
	{ok, TesStripped} = remove_ignored_c45(Tes, IgnoredAttributesPositions),
	Pos = length(Attributes) + 1,
	{Attributes, Class, [ fetch_class(tuple_to_list(TE), Pos) || TE <- TesStripped ]}.
	

read_tes(Filename, xml, NumOfAttributes) ->
	make_tes_tuples(data:file(Filename), NumOfAttributes, []);
read_tes(Filename, arff, NumOfAttributes) ->
	make_tes_tuples(data:file_arff(Filename), NumOfAttributes, []);
read_tes(Filename, c45, NumOfAttributes) ->
	make_tes_tuples(data:file(Filename), NumOfAttributes, []).

make_tes_tuples([{normal, Tuple}|T], Num, Acc) ->
	make_tes_tuples(T, Num, [Tuple|Acc]);
make_tes_tuples([{sparse, ListOfAttr}|T], Num, Acc) ->
	make_tes_tuples(T, Num, [erlang:make_tuple(Num, 0, fix_positions(ListOfAttr, []))|Acc]);
make_tes_tuples([], _, Acc) ->
	{ok, Acc}.

fix_positions([{Pos, Val}|T], Acc) ->
	fix_positions(T, [{Pos + 1, Val}|Acc]);
fix_positions([], Acc) ->
	Acc.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


fetch_class(TE, Pos) ->
	{Example, Category} = mllib_tools:exclude(TE, Pos),
	?DETAIL("~p ~p~n", [TE, Example]),
	{list_to_tuple(Example), Category}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Misc
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

supervisor_spawner(Pid, Algorithm, Fun, Args, Nodes, no_ets) ->
	supervisor_manager:start_link({Algorithm, Fun, Args, no_ets}, Nodes, self()),
	process_flag(trap_exit, true),
	receive
		{ok, stopped} -> Pid ! {ok, stopped}, ok;
		{ok, Result} -> Pid ! {ok, Result}, ok;
		{error, Reason} -> Pid ! {error, Reason}, error;
		{'EXIT', _From, Reason}-> unregister(node_provider), global:unregister_name(supervisor), exit(Reason)
	end.

supervisor_spawner(Pid, Algorithm, Fun, Args, Nodes) ->
	supervisor_manager:start_link({Algorithm, Fun, Args}, Nodes, self()),
	process_flag(trap_exit, true),
	receive
		{ok, stopped} -> Pid ! {ok, stopped}, ok;
		{ok, Result} -> Pid ! {ok, Result}, ok;
		{error, Reason} -> Pid ! {error, Reason}, error;
		{'EXIT', _From, Reason}-> unregister(node_provider), global:unregister_name(supervisor), exit(Reason)
	end.



