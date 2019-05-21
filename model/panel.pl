:- module(panel, [
  create_panel/2,
  create_panel/3,
  create_panel/4,
  create_panel/5,
  set_panel_property/3,
  show_panel/1,
  hide_panel/1,
  set_panel_label/2,
  set_panel_visible/2,
  set_panel_position/2,
  remove_panel/1,
  set_class_panel_property/3,
  show_class_panel/1,
  hide_class_panel/1,
  set_class_panel_label/2,
  set_class_panel_visible/2,
  set_class_panel_position/2,
  remove_class_panel/1
]).

:- use_module(graph, [edge/3, vertex/2, create_edge/3, create_vertex/2, replace_edge/2, replace_vertex/2, remove_edge/3, remove_vertex/2]).
:- use_module(class, [is_class/1, get_class_name/2]).
:- use_module('../representation/qualified_name', [generate_qualified_name/2]).

panel(QualifiedName) -->
  "panel:",
  qualified_name(QualifiedNameCharacters),
  { atom_codes(QualifiedName, QualifiedNameCharacters) }.

qualified_name([]) --> [].
qualified_name([Character|Rest]) -->
  [Character],
  qualified_name(Rest).

% Assertion Theorems
is_panel(Panel) :-
  vertex(panel, Panel),
  string_codes(Panel, Stream),
  phrase(panel(_), Stream).

is_panel_visible(Panel) :-
  get_panel_visibility(Panel, true).

% Search Theorems
get_panel_for_class(Class, Panel) :-
  get_panel_identifier(Class, Panel),
  is_panel(Panel).

get_class_for_panel(Panel, Class) :-
  get_panel_identifier(Class, Panel),
  is_class(Class).

% Property Theorems
get_panel_identifier(Class, Panel) :-
  atom_concat('panel:', Class, Panel).

get_panel_label(Panel, Label) :-
  is_panel(Panel), !,
  edge(Panel, label, Label).

get_panel_visibility(Panel, Visibility) :-
  is_panel(Panel), !,
  edge(Panel, visible, Visibility).

get_panel_position(Panel, Position) :-
  is_panel(Panel), !,
  edge(Panel, position, Position).

%% Transformation Theorems
% Validation Theorems
can_create_panel(Class) :-
  is_class(Class),
  \+ edge(Class, panel, _).

% Creation Theorems
create_panel(Class, Panel) :-
  get_class_name(Class, Name),
  create_panel(Class, Name, Panel).
create_panel(Class, Label, Panel) :-
  create_panel(Class, Label, true, Panel).
create_panel(Class, Label, Visibility, Panel) :-
  create_panel(Class, Label, Visibility, 0, Panel).
create_panel(Class, Label, Visibility, Position, Panel) :-
  can_create_panel(Class),
  get_panel_identifier(Class, PanelIdentifier),
  generate_qualified_name([PanelIdentifier], Panel),
  create_vertex(panel, Panel),
  create_edge(Class, panel, Panel),
  create_edge(Panel, label, Label),
  create_edge(Panel, visible, Visibility),
  create_edge(Panel, position, Position).

% Replacement Theorems
set_class_panel_property(Class, Property, Value) :-
  get_panel_for_class(Class, Panel),
  set_panel_property(Panel, Property, Value).
set_panel_property(Panel, Property, Value) :-
  is_panel(Panel),
  edge(Panel, Property, CurrentValue),
  replace_edge(edge(Panel, Property, CurrentValue), edge(Panel, Property, Value)).

show_class_panel(Class) :-
  set_class_panel_visible(Class, true).
show_panel(Panel) :-
  set_panel_visible(Panel, true).

hide_class_panel(Class) :-
  set_class_panel_visible(Class, false).
hide_panel(Panel) :-
  set_panel_visible(Panel, false).

set_class_panel_visible(Class, Visibility) :-
  set_class_panel_property(Class, visible, Visibility).
set_panel_visible(Panel, Visibility) :-
  set_panel_property(Panel, visible, Visibility).

set_class_panel_label(Class, Label) :-
  set_class_panel_property(Class, label, Label).
set_panel_label(Panel, Label) :-
  set_panel_property(Panel, label, Label).

set_class_panel_position(Class, Position) :-
  set_class_panel_property(Class, position, Position).
set_panel_position(Panel, Position) :-
  set_panel_property(Panel, position, Position).

% Removal Theorems
remove_class_panel(Class) :-
  get_panel_for_class(Class, Panel),
  remove_panel(Panel).
remove_panel(Panel) :-
  remove_vertex(panel, Panel),
  remove_edge(Panel, label, _),
  remove_edge(Panel, visible, _),
  remove_edge(Panel, position, _).
