:- module(transformer, [
  apply_transformation/3
]).

:- use_module(model/graph, [load_graph/1, create_use_for_name/2, create_root/2, find_source/2, repository/1, rewrite_graph/0]).
:- use_module(model/class, [get_class_name/2, create_class/6]).
:- use_module(model/attribute, [add_attribute/5, create_accessors/4]).
:- use_module(model/panel, [get_panel_for_class/2, create_panel/2, create_panel/3, create_panel/4, create_panel/5, show_class_panel/1, hide_class_panel/1, remove_class_panel/1, set_class_panel_label/2, set_class_panel_position/2]).
:- use_module(model/field, [create_field/3, create_field/4, create_field/5, create_field/6, show_field/2, hide_field/2, remove_field/2, set_field_label/3, set_field_position/3]).
:- use_module(integration/java, [inject_attribute/5, inject_getter/4, inject_setter/4, inject_class/7]).
:- use_module(arrays, [parse_array/2]).
:- use_module(representation/qualified_name, [qualified_name/3]).

% Attribute and Accessors
transformation('add-attribute', [Class, Modifiers, Type, Name]) :-
  parse_array(Modifiers, ModifiersList), !,
  add_attribute(Class, ModifiersList, Type, Name, Attribute),
  create_accessors(Class, Attribute, Type, Name),
  find_source(Attribute, File),
  get_class_name(Class, ClassName),
  inject_attribute(File, ClassName, ModifiersList, Type, Name),
  inject_getter(File, ClassName, ModifiersList, Name),
  inject_setter(File, ClassName, ModifiersList, Name).

% Class
transformation('create-class', [Modifiers, QualifiedName, Interfaces]) :-
  transformation('create-class', [Modifiers, QualifiedName, _, Interfaces]).
transformation('create-class', [Modifiers, QualifiedName, Parent, Interfaces]) :-
  parse_array(Modifiers, ModifiersList),
  qualified_name(QualifiedName, Package, Name),
  parse_array(Interfaces, InterfacesList), !,
  create_class(Package, ModifiersList, Name, Parent, InterfacesList, Class),
  create_use_for_name(Name, Use),
  create_root(vertex(class, Class), Use),
  repository(Repository),
  inject_class(Repository, Package, ModifiersList, Name, Parent, InterfacesList, SourceFile),
  create_edge(Class, source, SourceFile).

%% UI
% Panel
transformation('create-panel', [Class]) :-
  create_panel(Class, _).
transformation('create-panel', [Class, Label]) :-
  create_panel(Class, Label, _).
transformation('create-panel', [Class, Label, Visibility]) :-
  create_panel(Class, Label, Visibility, _).
transformation('create-panel', [Class, Label, Visibility, Position]) :-
  create_panel(Class, Label, Visibility, Position, _).

transformation('show-panel', [Class]) :-
  show_class_panel(Class).

transformation('hide-panel', [Class]) :-
  hide_class_panel(Class).

transformation('set-panel-label', [Class, Label]) :-
  set_class_panel_label(Class, Label).

transformation('set-panel-position', [Class, Position]) :-
  set_class_panel_position(Class, Position).

transformation('remove-panel', [Class]) :-
  remove_class_panel(Class).

% Field
transformation('create-field', [Class, Attribute]) :-
  get_panel_for_class(Class, Panel), !,
  create_field(Panel, Attribute, _).
transformation('create-field', [Class, Attribute, Label]) :-
  get_panel_for_class(Class, Panel), !,
  create_field(Panel, Attribute, Label, _).
transformation('create-field', [Class, Attribute, Label, Visibility]) :-
  get_panel_for_class(Class, Panel), !,
  create_field(Panel, Attribute, Label, Visibility, _).
transformation('create-field', [Class, Attribute, Label, Visibility, Position]) :-
  get_panel_for_class(Class, Panel), !,
  create_field(Panel, Attribute, Label, Visibility, Position, _).

transformation('show-field', [Class, Attribute]) :-
  show_field(Class, Attribute).

transformation('hide-field', [Class, Attribute]) :-
  hide_field(Class, Attribute).

transformation('set-field-label', [Class, Attribute, Label]) :-
  set_field_label(Class, Attribute, Label).

transformation('set-field-position', [Class, Attribute, Position]) :-
  set_field_position(Class, Attribute, Position).

transformation('remove-field', [Class, Attribute]) :-
  remove_field(Class, Attribute).

%% Transformation Application
apply_transformation(Graph, Transformation, Arguments) :-
  string(Transformation),
  atom_string(TransformationFunctor, Transformation), !,
  apply_transformation(Graph, TransformationFunctor, Arguments).
apply_transformation(Graph, Transformation, Arguments) :-
  load_graph(Graph),
  TransformationPredicate =.. [transformation, Transformation, Arguments], !,
  call(TransformationPredicate),
  rewrite_graph.
