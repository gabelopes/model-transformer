:- use_module(model/model).
:- use_module(transformer).

file("/Users/sap/Workspace/Parthenos/tests/CompanyGraph/Company.pl").

startC :-
  file(File),
  load_graph(File),
  find_class("Employee", Class),
  add_attribute(Class, [protected], boolean, "fired", Attribute),
  get_panel_for_class(Class, Panel),
  create_field(Panel, Attribute, "Is the employee fired?", true, 9, _),
  rewrite_graph.

startClassCreation :-
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

startasdasd :-
  file(File),
  apply_transformation(
    File,
    'add-attribute',
    [
      'com.company.Boss',
      '["public"]',
      'com.company.Employee',
      "assistant"
    ]
  ).

start_p1 :-
  file(File),
  apply_transformation(File, 'create-panel', ['com.company.Boss', "Boss Registration", true]).

start_p2 :-
  file(File),
  apply_transformation(File, 'remove-panel', ['com.company.Boss']).

start_p3 :-
  file(File),
  apply_transformation(File, 'create-panel', ['com.company.Boss', "Bosses"]).

start_p4 :-
  file(File),
  apply_transformation(File, 'hide-panel', ['com.company.Boss']).

start_p5 :-
  file(File),
  apply_transformation(File, 'set-panel-position', ['com.company.Boss', 3]).

start_p6 :-
  file(File),
  apply_transformation(File, 'set-panel-position', ['com.company.Boss', 4]).

start1 :-
  file(File),
  apply_transformation(File, 'create-field', ['com.company.Boss', "assistant", "Boss' assistant", true]).

start2 :-
  file(File),
  apply_transformation(File, 'remove-field', ['com.company.Boss', "assistant"]).

start3 :-
  file(File),
  apply_transformation(File, 'create-field', ['com.company.Boss', "assistant", "Assitant"]).

staasdasdrt :-
  file(File),
  apply_transformation(File, 'show-field', ['com.company.Boss', "assistant"]).

start :-
  file(File),
  apply_transformation(File, 'set-field-label', ['com.company.Boss', "assistant", "Assistenta"]).

startasdaasdasdsd :-
  file(File),
  apply_transformation(File, 'set-field-position', ['com.company.Boss', "assistant", 4]).
