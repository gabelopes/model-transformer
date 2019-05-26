:- use_module(model/model).
:- use_module(transformer).

file("C:/Users/Gabriel/Desktop/Company/Company.pl").

startC :-
  file(File),
  load_graph(File),
  find_class("Employee", Class),
  add_attribute(Class, [protected], boolean, "fired", Attribute),
  get_panel_for_class(Class, Panel),
  create_field(Panel, Attribute, "Is the employee fired?", true, 9, _),
  rewrite_graph.

start :-
  file(File),
  apply_transformation(
    File,
    'create-class',
    [
      '["public"]',
      'com.company.Boss',
      'com.company.Employee',
      '[]'
    ]
  ).
