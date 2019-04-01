:- module(environment, [
  get_arguments/1
]).

get_arguments(Arguments) :-
  current_prolog_flag(argv, Arguments).
