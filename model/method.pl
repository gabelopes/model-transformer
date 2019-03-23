:- module(method, [
	find_method_by_name/3,
	find_method/3,
	get_method_class/2,
	get_method_modifiers/2,
	get_method_return/2,
	get_method_name/2,
	get_method_parameters/2
]).

:- use_module(graph, [edge/3, vertex/2]).
:- use_module(common, [is_type/1, get_name/2, get_modifiers/2]).
:- use_module(class, [is_class/1, find_class/2]).

% Assertion Theorems
is_method(Method) :-
	vertex(method, Method),
	edge(_, method, Method).

% Search Theorems
find_method_by_name(Class, Name, Method) :-
	find_class(Class, ClassLabel),
	edge(Method, name, Name),
	vertex(method, Method),
	edge(ClassLabel, method, Method).

find_method(Class, Text, Method) :-
	atom(Text),
	find_class(Class, ClassLabel),
	vertex(method, Method),
	edge(ClassLabel, method, Method).
find_method(Class, Text, Method) :-
	string(Text),
	find_method_by_name(Class, Text, Method).

% Properties Theorems
get_method_class(Method, Class) :-
	edge(Class, method, Method),
	is_class(Class).

get_method_modifiers(Method, Modifiers) :-
	is_method(Method),
	get_modifiers(Method, Modifiers).

get_method_return(Method, Type) :-
	is_method(Method),
	edge(Method, return, Type).

get_method_name(Method, Name) :-
	is_method(Method),
	get_name(Method, Name).

get_method_parameters(Method, Parameters) :-
	is_method(Method),
	findall(Parameter, edge(Method, parameter, Parameter), Parameters).

% Transformation Theorems
can_add_method(Class, Return, Parameters) :-
	is_class(Class),
	is_type(Return).

add_method(Class, Name, Modifiers, Return, Parameters) :-
	can_add_method(Class, Return, Parameters).