:- module(modifier, [
  is_modifier/1
]).

:- use_module(graph, [vertex/2]).

is_modifier(Modifier) :-
  nonvar(Modifier),
  vertex(modifier, Modifier).
