:- use_module('model/model').

start :-
  load_graph("C:\\Users\\Gabriel\\Desktop\\HaStuff\\graph.pl"),
  find_class("Employee", Class),
  add_attribute(Class, [protected], boolean, "fired", Attribute),
  create_panel(Class, "Employee Creation", Panel),
  create_field(Panel, Attribute, "Is the employee fired?", true, 9, _),
  rewrite_graph.
