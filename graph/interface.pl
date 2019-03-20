:- module(interface, [
	find_interface_by_name/2,
	find_interface/2,
	get_interface_modifiers/2,
	get_interface_name/2,
	get_interface_package/2,
	get_interface_parents/2,
	get_interface_methods/2
]).

:- use_module(graph, [get_edge/3, get_vertex/2]).
:- use_module(entity).

% Assertion Theorems
is_interface(Label) :- get_vertex(interface, Label).

% Search Theorems
find_interface_by_name(Name, Interface) :-
	is_interface(Interface),
	get_edge(Interface, name, Name).

find_interface(Text, Interface) :-
	atom(Text),
	is_interface(Text),
	Interface = Text.
find_interface(Text, Interface) :-
	string(Text),
	find_interface_by_name(Text, Interface).

% Property Theorems
get_interface_modifiers(Interface, Modifiers) :-
	is_interface(Interface),
	get_modifiers(Interface, Modifiers).

get_interface_name(Interface, Name) :-
	is_interface(Interface),
	get_name(Interface, Name).

get_interface_package(Interface, Package) :-
	is_interface(Interface),
	get_package(Interface, Package).

get_interface_parents(Interface, Parents) :-
	is_interface(Interface),
	findall(Parent, get_edge(Interface, parent, Parent), Parents).

% Content Theorems
get_interface_methods(Text, Methods) :-
	find_interface(Text, Interface),
	findall(Method, get_edge(Interface, method, Method), Methods).