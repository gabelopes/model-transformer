:- module(qualified_name, [
  generate_qualified_name/3,
  qualified_name/3
]).

:- use_module(text, [join_atoms/3, join_strings/3]).
:- use_module('../model/graph', [vertex/2]).

is_qualified_name_available(QualifiedName) :-
  \+ vertex(_, QualifiedName).

generate_numbered_name(Name, 1, Name) :- !.
generate_numbered_name(Name, Number, NumberedName) :-
  atom_concat(Name, Number, NumberedName).

generate_qualified_name(Parent, Name, Number, QualifiedName) :-
  generate_numbered_name(Name, Number, NumberedName),
  join_atoms([Parent, NumberedName], '.', QualifiedName),
  is_qualified_name_available(QualifiedName).
generate_qualified_name(Parent, Name, Number, QualifiedName) :-
  NextNumber is Number + 1,
  generate_qualified_name(Parent, Name, NextNumber, QualifiedName).

generate_qualified_name(Parent, Name, QualifiedName) :-
  generate_qualified_name(Parent, Name, 1, QualifiedName), !.

qualified_name(QualifiedName, Package, Identifier) :-
  split_string(QualifiedName, ".", "", Parts),
  split_qualified_name(Parts, PackageParts, Identifier),
  join_strings(PackageParts, ".", Package).

split_qualified_name([], [], "").
split_qualified_name([Part], [], Part).
split_qualified_name([PartA|Rest], [PartA|Partial], Name) :-
  separate_qualified_name(Rest, Partial, Name).
