:- module(method, [
	is_method/1,
	find_method_by_name/3,
	find_method/3,
	get_method_class/2,
	get_method_modifiers/2,
	get_method_return/2,
	get_method_name/2,
	get_method_parameters/2,
	add_method/6
]).

:- use_module(graph, [edge/3, vertex/2, create_edge/3, create_vertex/2]).
:- use_module(common, [is_type/1, get_name/2, get_modifiers/2]).
:- use_module(class, [is_class/1, find_class/2]).
:- use_module(modifier, [is_modifier/1]).
:- use_module(parameter, [add_parameter/5]).
:- use_module('../representation/qualified_name').
:- use_module('../arrays').

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

%% Transformation Theorems
% Validation Theorems
modifiers_are_valid([]).
modifiers_are_valid([Modifier|Rest]) :-
	is_modifier(Modifier),
	modifiers_are_valid(Rest).

parameters_have_different_names([], _).
parameters_have_different_names([parameter(_, _, Name)|Rest], Parameters) :-
	\+ has_multiple_occurrences(parameter(_, _, Name), Parameters),
	parameters_have_different_names(Rest, Parameters).
parameters_have_different_names(Parameters) :-
	parameters_have_different_names(Parameters, Parameters).

can_add_method(Class, Modifiers, Return, Parameters) :-
	is_class(Class),
	is_type(Return),
	modifiers_are_valid(Modifiers), !,
	parameters_have_different_names(Parameters).

% Creation Theorems
create_modifiers_edges(_, []).
create_modifiers_edges(Method, [Modifier|Rest]) :-
	create_edge(Method, modifier, Modifier),
	create_modifiers_edges(Method, Rest).

create_parameters(_, []).
create_parameters(Method, [parameter(Modifiers, Type, Name)|Rest]) :-
	add_parameter(Method, Modifiers, Type, Name, _),
	create_parameters(Method, Rest).

add_method(Class, Modifiers, Return, Name, Parameters, Method) :-
	can_add_method(Class, Modifiers, Return, Parameters),
	generate_qualified_name(Class, Name, Method),
	create_vertex(method, Method),
	create_edge(Method, unsynchronized, Method),
	create_edge(Class, method, Method),
	create_modifiers_edges(Method, Modifiers),
	create_edge(Method, return, Return),
	create_edge(Method, name, Name),
	create_parameters(Method, Parameters).