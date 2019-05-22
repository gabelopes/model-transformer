:- module(field, [
  create_field/3,
  create_field/4,
  create_field/5,
  create_field/6,
  set_field_property/4,
  set_field_property/3,
  show_field/2,
  show_field/1,
  hide_field/2,
  hide_field/1,
  set_field_visible/3,
  set_field_visible/2,
  set_field_label/3,
  set_field_label/2,
  set_field_position/3,
  set_field_position/2,
  remove_field/2,
  remove_field/1
]).

:- use_module(graph, [edge/3, vertex/2, create_edge/3, create_vertex/2, replace_edge/2, replace_vertex/2, remove_edge/3, remove_vertex/2]).
:- use_module(attribute, [is_attribute/1, get_attribute_name/2]).
:- use_module('../representation/qualified_name', [generate_qualified_name/2]).
:- use_module(panel, [is_panel/1]).

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
  nonvar(Field),
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

get_class_field(Class, Name, Field) :-
  find_attribute_by_name(Class, Name, Attribute),
  get_field_identifier(Attribute, Field).

% Property Theorems
get_field_identifier(Attribute, Field) :-
  atom_concat('field:', Attribute, Field).

get_field_property(Field, Property, Value) :-
  is_field(Field), !,
  edge(Field, Property, Value).

get_field_label(Field, Label) :-
  get_field_property(Field, label, Label).

get_field_visibility(Field, Visibility) :-
  get_field_property(Field, visible, Visibility).

get_field_position(Field, Position) :-
  get_field_property(Field, position, Position).

%% Transformation Theorems
% Validation Theorems
can_create_field(Panel, Attribute) :-
  is_panel(Panel),
  is_attribute(Attribute),
  \+ edge(Attribute, field, _).

% Creation Theorems
create_field(Panel, Attribute, Field) :-
  get_attribute_name(Attribute, Name),
  create_field(Panel, Attribute, Name, Field).
create_field(Panel, Attribute, Label, Field) :-
  create_field(Panel, Attribute, Label, true, Field).
create_field(Panel, Attribute, Label, Visibility, Field) :-
  create_field(Panel, Attribute, Label, Visibility, 0, Field).
create_field(Panel, Attribute, Label, Visibility, Position, Field) :-
  can_create_field(Panel, Attribute),
  get_field_identifier(Attribute, FieldIdentifier),
  generate_qualified_name([FieldIdentifier], Field),
  create_vertex(field, Field),
  create_edge(Panel, field, Field),
  create_edge(Attribute, field, Field),
  create_edge(Field, label, Label),
  create_edge(Field, visible, Visibility),
  create_edge(Field, position, Position).

% Replacement Theorems
set_field_property(Class, Attribute, Property, Value) :-
  get_class_field(Class, Attribute, Field),
  set_field_property(Field, Property, Value).
set_field_property(Field, Property, Value) :-
  is_field(Field),
  edge(Field, Property, CurrentValue),
  replace_edge(edge(Field, Property, CurrentValue), edge(Field, Property, Value)).

show_field(Class, Attribute) :-
  set_field_visible(Class, Attribute, true).
show_field(Field) :-
  set_field_visible(Field, true).

hide_field(Class, Attribute) :-
  set_field_visible(Class, Attribute, false).
hide_field(Field) :-
  set_field_visible(Field, false).

set_field_visible(Class, Attribute, Visibility) :-
  set_field_property(Class, Attribute, visible, Visibility).
set_field_visible(Field, Visibility) :-
  set_field_property(Field, visible, Visibility).

set_field_label(Class, Attribute, Label) :-
  set_field_property(Class, Attribute, label, Label).
set_field_label(Field, Label) :-
  set_field_property(Field, label, Label).

set_field_position(Class, Attribute, Position) :-
  set_field_property(Class, Attribute, position, Position).
set_field_position(Field, Position) :-
  set_field_property(Field, position, Position).

% Removal Theorems
remove_field(Class, Attribute) :-
  get_class_field(Class, Attribute, Field),
  remove_field(Field).
remove_field(Field) :-
  remove_vertex(field, Field),
  remove_edge(Field, label, _),
  remove_edge(Field, visible, _),
  remove_edge(Field, position, _).
