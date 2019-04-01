:- module(class, [
	is_class/1,
	find_class_by_name/2,
	find_class/2,
	get_class_modifiers/2,
	get_class_name/2,
	get_class_package/2,
	get_class_parent/2,
	get_class_interfaces/2,
	get_class_attributes/2,
	get_class_methods/2
]).

:- use_module(graph, [edge/3, vertex/2]).
:- use_module(common, [get_name/2, get_package/2, get_modifiers/2]).

% Assertion Theorems
is_class(Label) :-
	vertex(class, Label).

% Search Theorems
find_class_by_name(Name, Class) :-
	is_class(Class),
	edge(Class, name, Name).

find_class(Text, Class) :-
	atom(Text),
	is_class(Text),
	Class = Text.
find_class(Text, Class) :-
	string(Text),
	find_class_by_name(Text, Class).

% Property Theorems
get_class_modifiers(Class, Modifiers) :-
	is_class(Class),
	get_modifiers(Class, Modifiers).

get_class_name(Class, Name) :-
	is_class(Class),
	get_name(Class, Name).

get_class_package(Class, Package) :-
	is_class(Class),
	get_package(Class, Package).

get_class_parent(Class, Parent) :-
	is_class(Class),
	edge(Class, parent, Parent).

get_class_interfaces(Class, Interfaces) :-
	is_class(Class),
	findall(Interface, edge(Class, interface, Interface), Interfaces).

% Content Theorems
get_class_attributes(Text, Attributes) :-
	find_class(Text, Class),
	findall(Attribute, edge(Class, attribute, Attribute), Attributes).

get_class_methods(Text, Methods) :-
	find_class(Text, Class),
	findall(Method, edge(Class, method, Method), Methods).