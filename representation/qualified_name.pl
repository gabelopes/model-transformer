:- module(qualified_name, [
  generate_qualified_name/2,
  qualified_name/3
]).

:- use_module(text, [join_atoms/3, join_strings/3]).
:- use_module('../model/graph', [vertex/2]).

is_qualified_name_available(QualifiedName) :-
  \+ vertex(_, QualifiedName).

generate_numbered_name(Name, 1, Name) :- !.
generate_numbered_name(Name, Number, NumberedName) :-
  atom_concat(Name, Number, NumberedName).

generate_qualified_name(Name, Number, QualifiedName) :-
  generate_numbered_name(Name, Number, QualifiedName),
  is_qualified_name_available(QualifiedName).
generate_qualified_name(Name, Number, QualifiedName) :-
  NextNumber is Number + 1,
  generate_qualified_name(Name, NextNumber, QualifiedName).

generate_qualified_name(Parts, QualifiedName) :-
  join_atoms(Parts, '.', JoinedParts),
  generate_qualified_name(JoinedParts, 1, QualifiedName), !.

qualified_name(QualifiedName, Package, Identifier) :-
  split_string(QualifiedName, ".", "", Parts),
  split_qualified_name(Parts, PackageParts, Identifier),
  join_strings(PackageParts, ".", Package).

split_qualified_name([], [], "").
split_qualified_name([Part], [], Part).
split_qualified_name([PartA|Rest], [PartA|Partial], Name) :-
  separate_qualified_name(Rest, Partial, Name).
