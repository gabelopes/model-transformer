% Assertion Theorems
is_parameter(Parameter) :-
	vertex(parameter, Parameter),
	edge(_, parameter, Parameter).

% Search Theorems
find_parameter_by_name(Class, Method, Name, Parameter) :-
	find_method(Class, Method, MethodLabel),
	edge(Parameter, name, Name),
	vertex(parameter, Parameter),
	edge(MethodLabel, parameter, Parameter).

% Properties Theorems
get_parameter_modifiers(Parameter, Modifiers) :-
	is_parameter(Parameter),
	get_modifiers(Parameter, Modifiers).

get_parameter_type(Parameter, Type) :-
	is_parameter(Parameter),
	get_type(Type).

get_parameter_name(Parameter, Name) :-
	is_parameter(Parameter),
	get_name(Parameter, Name).