:- use_module(transformer).

file("/Users/sap/Workspace/Parthenos/testing/people-graph/graph.pl").

startb :-
  file(File),
  apply_transformation(
    File,
    'create-class',
    [
      '["public"]',
      'LMS.Magazine',
      'LMS.Book',
      '[]'
    ]
  ).

start :-
  file(File),
  apply_transformation(
    File,
    'add-attribute',
    [
      'com.company.Person',
      '["private"]',
      'String',
      "nickname"
    ]
  ).
