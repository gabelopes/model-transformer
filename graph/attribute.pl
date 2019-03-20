:- module(attribute, [
	find_attribute_by_name/3,
	get_attribute_class/2,
	get_attribute_modifiers/2,
	get_attribute_type/2,
	get_attribute_name/2
]).

:- use_module(graph, [get_edge/3, get_vertex/2]).
:- use_module(entity).

% Assertion Theorems
is_attribute(Attribute) :-
	get_vertex(attribute, Attribute),
	get_edge(_, attribute, Attribute).

% Search Theorems
find_attribute_by_name(Class, Name, Attribute) :-
	find_class(Class, ClassLabel),
	get_edge(Attribute, name, Name),
	get_vertex(attribute, Attribute),
	get_edge(ClassLabel, attribute, Attribute).

% Properties Theorems
get_attribute_class(Attribute, Class) :-
	get_edge(Class, attribute, Attribute),
	is_class(Class).

get_attribute_modifiers(Attribute, Modifiers) :-
	is_attribute(Attribute),
	get_modifiers(Attribute, Modifiers).

get_attribute_type(Attribute, Type) :-
	is_attribute(Attribute),
	get_type(Type).

get_attribute_name(Attribute, Name) :-
	is_attribute(Attribute),
	get_name(Attribute, Name).