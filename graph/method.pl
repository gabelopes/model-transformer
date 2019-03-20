:- module(method, [
	find_method_by_name/3,
	find_method/3,
	get_method_class/2,
	get_method_modifiers/2,
	get_method_return/2,
	get_method_name/2,
	get_method_parameters/2
]).

:- use_module(graph, [get_edge/3, get_vertex/2]).
:- use_module(entity).

% Assertion Theorems
is_method(Method) :-
	get_vertex(method, Method),
	get_edge(_, method, Method).

% Search Theorems
find_method_by_name(Class, Name, Method) :-
	find_class(Class, ClassLabel),
	get_edge(Method, name, Name),
	get_vertex(method, Method),
	get_edge(ClassLabel, method, Method).

find_method(Class, Text, Method) :-
	atom(Text),
	find_class(Class, ClassLabel),
	get_vertex(method, Method),
	get_edge(ClassLabel, method, Method).
find_method(Class, Text, Method) :-
	string(Text),
	find_method_by_name(Class, Text, Method).

% Properties Theorems
get_method_class(Method, Class) :-
	get_edge(Class, method, Method),
	is_class(Class).

get_method_modifiers(Method, Modifiers) :-
	is_method(Method),
	get_modifiers(Method, Modifiers).

get_method_return(Method, Type) :-
	is_method(Method),
	get_edge(Method, return, Type).

get_method_name(Method, Name) :-
	is_method(Method),
	get_name(Method, Name).

get_method_parameters(Method, Parameters) :-
	is_method(Method),
	findall(Parameter, get_edge(Method, parameter, Parameter), Parameters).