:- use_module('model/model').

start :-
  load_graph("/Users/sap/Downloads/CompanyGraph/graph.pl"),
  find_class("Employee", Class),
  add_attribute(Class, [protected], boolean, "fired", _),
  rewrite_graph.
