:- module(qualified_name, [
  generate_qualified_name/2,
  generate_file_name/3,
  qualified_name/3
]).

:- use_module(text, [join_atoms/3, join_strings/3]).
:- use_module('../model/graph', [vertex/2]).

% Qualified Name Generation
is_qualified_name_available(QualifiedName) :-
  \+ vertex(_, QualifiedName).

generate_qualified_name(Parts, QualifiedName) :-
  join_atoms(Parts, '.', JoinedParts),
  generate_available_name(JoinedParts, is_qualified_name_available, QualifiedName), !.

split_qualified_name([], [], "").
split_qualified_name([Part], [], Part).
split_qualified_name([PartA|Rest], [PartA|Partial], Name) :-
  split_qualified_name(Rest, Partial, Name).

qualified_name(QualifiedName, Package, Identifier) :-
  split_string(QualifiedName, ".", "", Parts),
  split_qualified_name(Parts, PackageParts, Identifier),
  join_strings(PackageParts, ".", Package).

% File Name Generation
is_file_name_available(FileName) :-
  \+ exists_file(FileName).

generate_file_name(Folder, Name, FileName) :-
  atom_concat('./', Name, RelativeName),
  relative_file_name(Path, Folder, RelativeName),
  generate_available_name(Path, is_file_name_available, FileName).

% Available Name Generation
generate_numbered_name(Name, 1, Name) :- !.
generate_numbered_name(Name, Number, NumberedName) :-
  atom_concat(Name, Number, NumberedName).

generate_available_name(Name, Condition, Number, AvailableName) :-
  generate_numbered_name(Name, Number, AvailableName),
  call(Condition, AvailableName).
generate_available_name(Name, Condition, Number, AvailableName) :-
  NextNumber is Number + 1,
  generate_available_name(Name, Condition, NextNumber, AvailableName).

generate_available_name(Name, Condition, AvailableName) :-
  generate_available_name(Name, Condition, 1, AvailableName).
