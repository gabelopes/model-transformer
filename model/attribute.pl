:- module(attribute, [
	find_attribute_by_name/3,
	get_attribute_class/2,
	get_attribute_modifiers/2,
	get_attribute_type/2,
	get_attribute_name/2,
	add_attribute/4
]).

:- use_module(graph, [edge/3, vertex/2, create_edge/3, create_vertex/2]).
:- use_module(common, [is_type/1, get_name/2, get_type/2, get_modifiers/2]).
:- use_module(class, [is_class/1, find_class/2]).
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
attribute_exists(Class, Name) :-
	edge(Class, attribute, Attribute),
	edge(Attribute, name, Name).

can_add_attribute(Class, Name, Type) :-
	is_class(Class),
	is_type(Type),
	\+ attribute_exists(Class, Name).

add_attribute(Class, Name, Type, Attribute) :-
	can_add_attribute(Class, Name, Type),
	generate_qualified_name(Class, Name, Attribute),
	create_vertex(attribute, Attribute),
	create_edge(Attribute, unsynchronized, Attribute),
	create_edge(Class, attribute, Attribute),
	create_edge(Attribute, modifier, private),
	create_edge(Attribute, name, Name),
	create_edge(Attribute, type, Type).