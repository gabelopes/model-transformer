:- module(qualified_name, [
	generate_qualified_name/3
]).

:- use_module(text, [join_atoms/3]).
:- use_module('../graph/graph', [vertex/2]).

is_qualified_name_available(QualifiedName) :-
	\+ vertex(_, QualifiedName).

generate_numbered_name(Name, 0, Name).
generate_numbered_name(Name, Number, NumberedName) :-
	atom_concat(Name, Number, NumberedName).

generate_qualified_name(Parent, Name, Number, QualifiedName) :-
	generate_numbered_name(Name, Number, NumberedName),
	join_atoms([Parent, NumberedName], '.', Y),
	is_qualified_name_available(QualifiedName).
generate_qualified_name(Parent, Name, Number, QualifiedName) :-
	NextNumber is Number + 1,
	generate_qualified_name(Parent, Name, NextNumber, QualifiedName).

generate_qualified_name(Parent, Name, QualifiedName) :-
	generate_qualified_name(Parent, Name, 0, QualifiedName).