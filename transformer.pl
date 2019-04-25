:- module(transformer, [
  apply_transformation/2
]).

:- use_module(cli, [find_option/3]).

specification(add_attribute, [
  [opt('class'), shortflags(['c']), longflags(['class']), type(atom)],
  [opt('modifiers'), shortflags(['m']), longflags(['modifiers']), type(atom)]
]).

apply_transformation(Graph, Transformation, Arguments) :-
