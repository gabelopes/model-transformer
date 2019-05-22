:- module(transformer, [
  apply_transformation/3
]).

:- use_module(model/graph, [load_graph/1, find_source/3, rewrite_graph/0]).
:- use_module(model/class, [get_class_name/2, create_class/6]).
:- use_module(model/attribute, [add_attribute/5, create_accessors/4]).
:- use_module(integration/java, [inject_attribute/5, inject_getter/4, inject_setter/4]).
:- use_module(arrays, [parse_array/2]).
:- use_module(representation/qualified_name, [qualified_name/3]).

add_attribute(Class, Modifiers, Type, Name) :-
  parse_array(Modifiers, ModifiersList),
  add_attribute(Class, ModifiersList, Type, Name, Attribute),
  create_accessors(Class, Attribute, Type, Name),
  find_source(Attribute, File),
  get_class_name(Class, ClassName),
  inject_attribute(File, ClassName, ModifiersList, Type, Name),
  inject_getter(File, ClassName, ModifiersList, Name),
  inject_setter(File, ClassName, ModifiersList, Name).

create_class(Modifiers, QualifiedName, Interfaces) :-
  create_class(Modifiers, QualifiedName, _, Interfaces).
create_class(Modifiers, QualifiedName, Parent, Interfaces) :-
  parse_array(Modifiers, ModifiersList),
  qualified_name(QualifiedName, Package, Name),
  parse_array(Interfaces, InterfacesList),
  create_class(Package, ModifiersList, Name, Parent, InterfacesList, _).

apply_transformation(Graph, Transformation, Arguments) :-
  string(Transformation),
  atom_string(TransformationFunctor, Transformation), !,
  apply_transformation(Graph, TransformationFunctor, Arguments).
apply_transformation(Graph, Transformation, Arguments) :-
  load_graph(Graph),
  TransformationPredicate =.. [Transformation|Arguments], !,
  call(TransformationPredicate),
  rewrite_graph.
