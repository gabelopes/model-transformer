:- module(parameter, [
	find_parameter_by_name/4,
	get_parameter_modifiers/2,
	get_parameter_type/2,
	get_parameter_name/2
]).

:- use_module(graph, [get_edge/3, get_vertex/2]).
:- use_module(entity).

% Assertion Theorems
is_parameter(Parameter) :-
	get_vertex(parameter, Parameter),
	get_edge(_, parameter, Parameter).

% Search Theorems
find_parameter_by_name(Class, Method, Name, Parameter) :-
	find_method(Class, Method, MethodLabel),
	get_edge(Parameter, name, Name),
	get_vertex(parameter, Parameter),
	get_edge(MethodLabel, parameter, Parameter).

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