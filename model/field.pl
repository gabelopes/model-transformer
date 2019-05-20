:- module(field, [
  create_field/2,
  create_field/3,
  create_field/4,
  create_field/5
]).

:- use_module(graph, [edge/3, vertex/2, create_edge/3, create_vertex/2]).
:- use_module(attribute, [is_attribute/1, get_attribute_name/2]).

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
get_field_for_attribute(Attribute, Field) :-
  get_field_identifier(Attribute, Field),
  is_field(Field).

get_attribute_for_field(Field, Attribute) :-
  get_field_identifier(Attribute, Field),
  is_attribute(Attribute).

% Property Theorems
get_field_identifier(Attribute, Field) :-
  atom_concat('field:', Attribute, Field).

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
can_create_field(Attribute) :-
  is_attribute(Attribute),
  \+ edge(Attribute, field, Field),
  is_field(Field).

% Creation Theorems
create_field(Attribute, Field) :-
  get_attribute_name(Attribute, Name),
  create_field(Attribute, Name, Field).
create_field(Attribute, Label, Field) :-
  create_field(Attribute, Label, true, Field).
create_field(Attribute, Label, Visibility, Field) :-
  create_field(Attribute, Label, Visibility, 0, Field).
create_field(Attribute, Label, Visibility, Position, Field) :-
  can_create_field(Attribute),
  get_field_identifier(Attribute, FieldQualifiedName),
  generate_qualified_name('', FieldQualifiedName, Field),
  create_vertex(field, Field),
  create_edge(Field, label, Label),
  create_edge(Field, visible, Visibility),
  create_edge(Field, position, Position).

% Replacement Theorems
show_field(Attribute) :-


show_field(Field) :-


remove_field()
