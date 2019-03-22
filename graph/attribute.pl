:- module(attribute, [
	find_attribute_by_name/3,
	get_attribute_class/2,
	get_attribute_modifiers/2,
	get_attribute_type/2,
	get_attribute_name/2,
	add_attribute/4
]).

:- use_module(graph, [edge/3, vertex/2, create_edge/3, create_vertex/2]).
:- use_module(entity).
:- use_module(class, [is_class/1]).
:- use_module('../representation/qualified_name').

% Assertion Theorems
is_attribute(Attribute) :-
	vertex(attribute, Attribute),
	edge(_, attribute, Attribute).

% Search Theorems
find_attribute_by_name(Class, Name, Attribute) :-
	find_class(Class, ClassLabel),
	edge(Attribute, name, Name),
	vertex(attribute, Attribute),
	edge(ClassLabel, attribute, Attribute).

% Properties Theorems
get_attribute_class(Attribute, Class) :-
	edge(Class, attribute, Attribute),
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

% Transformations
add_attribute(Class, Name, Type, Attribute) :-
	is_class(Class),
	is_type(Type),
	generate_qualified_name(Class, Name, Attribute),
	create_vertex(attribute, Attribute),
	create_edge(Attribute, unsynchronized, Attribute),
	create_edge(Class, attribute, Attribute),
	create_edge(Attribute, modifier, private),
	create_edge(Attribute, name, Name),
	create_edge(Attribute, type, Type),
	generate_qualified_name(Class, Name, Attribute2),
	write(Attribute2).