:- module(transformer, [
  apply_transformation/3
]).

:- use_module(model/graph, [load_graph/1, synchronize_graph/0, find_source/3]).
:- use_module(model/class, [get_class_name/2, create_class/6]).
:- use_module(model/attribute, [add_attribute/5, create_accessors/4]).
:- use_module(integration/java, [inject_attribute/5, inject_getter/4, inject_setter/4]).
:- use_module(representation/text, [atoms_strings/2]).

parse_list("", []).
parse_list(Argument, Modifiers) :-
  split_string(Argument, ",", "", List),
  atoms_strings(Modifiers, List).

add_attribute(Class, Type, Name) :-
  add_attribute(Class, "", Type, Name).
add_attribute(Class, ModifiersString, Type, Name) :-
  parse_list(ModifiersString, Modifiers),
  add_attribute(Class, Modifiers, Type, Name, Attribute),
  create_accessors(Class, Attribute, Type, Name),
  find_source(Attribute, File),
  get_class_name(Class, ClassName),
  inject_attribute(File, ClassName, Modifiers, Type, Name),
  inject_getter(File, ClassName, Modifiers, Name),
  inject_setter(File, ClassName, Modifiers, Name).

create_class(Package, ModifiersString, Name, Parent, Interfaces) :-
  parse_list(ModifiersString, Modifiers),
  create_class(Package, Modifiers, Name, Parent, Interfaces). %%%%%%%%%%%%%%%%%%% CONTINUES

apply_transformation(Graph, Transformation, Arguments) :-
  string(Transformation),
  atom_string(TransformationFunctor, Transformation), !,
  apply_transformation(Graph, TransformationFunctor, Arguments).
apply_transformation(Graph, Transformation, Arguments) :-
  load_graph(Graph),
  TransformationPredicate =.. [Transformation|Arguments], !,
  call(TransformationPredicate),
  synchronize_graph.
