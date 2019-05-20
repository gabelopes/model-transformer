:- use_module('model/model').

start :-
  load_graph("C:\\Users\\Gabriel\\Desktop\\HaStuff\\graph.pl"),
  find_class("Employee", Class),
  add_attribute(Class, [protected], boolean, fired, _),
  rewrite_graph.
