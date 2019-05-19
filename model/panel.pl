:- module(panel, [
]).

:- use_module(graph, [edge/3, vertex/2, create_edge/3, create_vertex/2]).
:- use_module(class, [is_class/1]).

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

get_panel_label(Panel, Label),
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
  \+ edge(Class, panel, Panel),
  is_panel(Panel).

% Creation Theorems
create_panel(Class, Label, Visibility, Position, Panel) :-
  can_create_panel(Class),
  get_panel_identifier(Class, PanelQualifiedName),
  generate_qualified_name('', PanelQualifiedName, Panel),
  create_vertex(panel, Panel),
  create_edge(Panel, label, Label),
  create_edge(Panel, visible, Visibility),
  create_edge(Panel, position, Position).
