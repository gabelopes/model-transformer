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
get_type(Label, Type) :-
	is_vertex(Label),
	edge(Label, type, Type).

% Modifiable Vertices Theorems
get_modifiers(Label, Modifiers) :-
	is_vertex(Label),
	findall(Modifier, edge(Label, modifier, Modifier), Modifiers).