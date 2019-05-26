:- module(transformer, [
  apply_transformation/3
]).

:- use_module(model/graph).
:- use_module(model/class, [get_class_name/2, create_class/6]).
:- use_module(model/attribute, [add_attribute/5, create_accessors/4]).
:- use_module(model/panel).
:- use_module(model/field).
:- use_module(integration/java).
:- use_module(integration/pojoui).
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
transformation('create-panel', [Class|Arguments]) :-
  CreatePanelGoal =.. ['create_panel', Class|Arguments],
  call(CreatePanelGoal, _),
  find_source(vertex(class, Class), SourceFile),
  InjectPanelGoal =.. ['inject_panel', SourceFile, Class|Arguments],
  class(InjectPanelGoal).

transformation('show-panel', [Class]) :-
  show_class_panel(Class),
  find_source(vertex(class, Class), SourceFile),
  inject_panel_visibility(SourceFile, Class, true).

transformation('hide-panel', [Class]) :-
  hide_class_panel(Class),
  find_source(vertex(class, Class), SourceFile),
  inject_panel_visibility(SourceFile, Class, false).

transformation('set-panel-label', [Class, Label]) :-
  set_class_panel_label(Class, Label),
  find_source(vertex(class, Class), SourceFile),
  inject_panel_label(SourceFile, Class, Label).

transformation('set-panel-position', [Class, Position]) :-
  set_class_panel_position(Class, Position),
  find_source(vertex(class, Class), SourceFile),
  inject_panel_position(SourceFile, Class, Position).

transformation('remove-panel', [Class]) :-
  remove_class_panel(Class),
  find_source(vertex(class, Class), SourceFile),
  inject_panel_deletion(SourceFile, Class).

% Field
transformation('create-field', [Class|Arguments]) :-
  get_panel_for_class(Class, Panel), !,
  CreateFieldGoal =.. ['create-field', Panel|Arguments],
  call(CreateFieldGoal, _),
  find_source(vertex(class, Class), SourceFile),
  InjectFieldGoal =.. ['inject_field', SourceFile, Class|Arguments],
  class(InjectPanelGoal).

transformation('show-field', [Class, Attribute]) :-
  show_field(Class, Attribute),
  find_source(vertex(class, Class), SourceFile),
  inject_field_visibility(SourceFile, Class, Attribute, true).

transformation('hide-field', [Class, Attribute]) :-
  hide_field(Class, Attribute),
  find_source(vertex(class, Class), SourceFile),
  inject_field_visibility(SourceFile, Class, Attribute, false).

transformation('set-field-label', [Class, Attribute, Label]) :-
  set_field_label(Class, Attribute, Label),
  find_source(vertex(class, Class), SourceFile),
  inject_field_label(SourceFile, Class, Attribute, Label).

transformation('set-field-position', [Class, Attribute, Position]) :-
  set_field_position(Class, Attribute, Position),
  find_source(vertex(class, Class), SourceFile),
  inject_field_position(SourceFile, Class, Attribute, Position).

transformation('remove-field', [Class, Attribute]) :-
  remove_field(Class, Attribute),
  find_source(vertex(class, Class), SourceFile),
  inject_field_deletion(SourceFile, Attribute, Class).

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
