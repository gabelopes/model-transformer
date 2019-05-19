:- module(field, [
]).

:- use_module(graph, [edge/3, vertex/2, create_edge/3, create_vertex/2]).
:- use_module(class, [is_class/1]).

field(QualifiedName) -->
  "field:",
  qualified_name(QualifiedNameCharacters),
  { atom_codes(QualifiedName, QualifiedNameCharacters) }.

qualified_name([]) --> [].
qualified_name([Character|Rest]) -->
  [Character],
  qualified_name(Rest).

% Assertion Theorems
is_field(Field) :-
  vertex(field, Field),
  string_codes(Field, Stream),
  phrase(field(_), Stream).

is_field_visible(Field) :-
  get_field_visibility(Field, true).

% Search Theorems
get_field_for_class(Class, Field) :-
  get_field_identifier(Class, Field),
  is_field(Field).

get_class_for_field(Field, Class) :-
  get_field_identifier(Class, Field),
  is_class(Class).

% Property Theorems
get_field_identifier(Class, Field) :-
  atom_concat('field:', Class, Field).

get_field_label(Field, Label),
  is_field(Field), !,
  edge(Field, label, Label).

get_field_visibility(Field, Visibility) :-
  is_field(Field), !,
  edge(Field, visible, Visibility).

get_field_position(Field, Position) :-
  is_field(Field), !,
  edge(Field, position, Position).

%% Transformation Theorems
% Validation Theorems
can_create_field(Class) :-
  is_class(Class),
  \+ edge(Class, field, Field),
  is_field(Field).

% Creation Theorems
create_field(Class, Label, Visibility, Position, Field) :-
  can_create_field(Class),
  get_field_identifier(Class, FieldQualifiedName),
  generate_qualified_name('', FieldQualifiedName, Field),
  create_vertex(field, Field),
  create_edge(Field, label, Label),
  create_edge(Field, visible, Visibility),
  create_edge(Field, position, Position).
