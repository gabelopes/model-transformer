:- use_module(model/model).
:- use_module(transformer).

start :-
  load_graph("/Users/sap/Workspace/Parthenos/tests/Company/graph.pl"),
  find_class("Employee", Class),
  add_attribute(Class, [protected], boolean, "fired", Attribute),
  % create_panel(Class, "Employee Creation", Panel),
  create_field(Panel, Attribute, "Is the employee fired?", true, 9, _),
  rewrite_graph.

startA :-
  apply_transformation(
    '/Users/sap/Workspace/Parthenos/tests/Company/graph.pl',
    'create_class',
    [
      '["public", "final"]',
      'br.unisinos.parthenos.test.Child',
      'com.company.Person',
      '[]'
    ]
  ).
