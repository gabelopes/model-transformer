:- module(transformer, [
  apply_transformation/3
]).

:- use_module(model/graph, [load_graph/1, synchronize_graph/0]).
:- use_model(model/attribute, [add_attribute/5]).

add_attribute([Class, Modifiers, Type, Name]) :-
  add_attribute(Class, Modifiers, Type, Name, _).

apply_transformation(Graph, Transformation, Arguments) :-
  string(Transformation),
  atom_string(TransformationFunctor, Transformation), !,
  apply_transformation(Graph, TransformationFunctor, Arguments).
apply_transformation(Graph, Transformation, Arguments) :-
  load_graph(Graph),
  TransformationPredicate =.. [Transformation, Arguments], !,
  call(TransformationPredicate),
  synchronize_graph.
