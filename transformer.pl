:- module(transformer, [
  apply_transformation/3
]).

:- use_module(model/graph, [load_graph/1, synchronize_graph/0, find_owner/2]).
:- use_module(model/attribute, [add_attribute/5]).
:- use_module(integration/java, [inject_attribute/5, inject_getter/4, inject_setter/4]).

add_attribute([Class, Modifiers, Type, Name]) :-
  add_attribute(Class, Modifiers, Type, Name, Attribute),
  create_accessors(Class, Attribute, Type, Name),
  find_owner(Attribute, File),
  inject_attribute(File, Class, Modifiers, Type, Name),
  inject_getter(File, Class, Modifiers, Name),
  inject_setter(File, Class, Modifiers, Name).

apply_transformation(Graph, Transformation, Arguments) :-
  string(Transformation),
  atom_string(TransformationFunctor, Transformation), !,
  apply_transformation(Graph, TransformationFunctor, Arguments).
apply_transformation(Graph, Transformation, Arguments) :-
  load_graph(Graph),
  TransformationPredicate =.. [Transformation, Arguments], !,
  call(TransformationPredicate),
  synchronize_graph.
