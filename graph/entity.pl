:- module(entity, [
	get_name/2,
	get_package/2,
	is_type/1,
	get_type/2,
	find_type_by_name/2,
	get_modifiers/2
]).

:- use_module(graph, [edge/3, vertex/2]).

% Assertion Theorems
is_vertex(Label) :- vertex(_, Label).

% Named Vertices Theorems
get_name(Label, Name) :-
	is_vertex(Label),
	edge(Label, name, Name).

% Packaged Vertices Theorems
get_package(Label, Package) :-
	is_vertex(Label),
	edge(Label, package, Package).

% Typed Vertices Theorems
type(unknown_type).
type(class).
type(interface).
type(primitive).
is_type(Type) :-
	vertex(Descriptor, Type),
	type(Descriptor).

get_type(Label, Type) :-
	is_vertex(Label),
	edge(Label, type, Type).

find_type_by_name(Name, Type) :-
	edge(Type, name, Name),
	vertex(Descriptor, Type),
	type(Descriptor).

% Modifiable Vertices Theorems
get_modifiers(Label, Modifiers) :-
	is_vertex(Label),
	findall(Modifier, edge(Label, modifier, Modifier), Modifiers).