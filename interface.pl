% Assertion Theorems
is_interface(Label) :- vertex(interface, Label).

% Search Theorems
find_interface_by_name(Name, Interface) :-
	is_interface(Interface),
	edge(Interface, name, Name).

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
	findall(Parent, edge(Interface, parent, Parent), Parents).

% Content Theorems
get_interface_methods(Text, Methods) :-
	find_interface(Text, Interface),
	findall(Method, edge(Interface, method, Method), Methods).