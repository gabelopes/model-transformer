:- module(cli, [
  find_option/3
]).

:- use_module(library(optparse)).
:- use_module('system/environment', [get_arguments/1]).
:- use_module(transformer, [apply_transformation/3]).

get_specification([
  [opt('graph'), shortflags(['g']), longflags(['graph']), type(atom)],
  [opt('transformation'), shortflags(['t']), longflags(['transformation']), type(atom)]
]).

find_option([], _, _) :- fail.
find_option([Option|_], Key, Value) :-
  Option =.. [Key, Value].
find_option([_|Rest], Key, Value) :-
  find_option(Rest, Key, Value).

get_graph(Options, Transformation) :-
  find_option(Options, 'graph', Transformation).

get_transformation(Options, Transformation) :-
  find_option(Options, 'transformation', Transformation).

start :-
  get_arguments(Arguments),
  get_specification(Specification),
  opt_parse(Specification, Arguments, Options, Positional),
  get_graph(Options, Graph),
  get_transformation(Options, Transformation),
  apply_transformation(Graph, Transformation, Positional),
  halt.
