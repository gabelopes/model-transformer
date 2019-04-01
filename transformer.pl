:- use_module('system/environment', [get_arguments/1]).
:- use_module('system/console', [write_all/1]).

start :-
  get_arguments(Arguments),
  write_all(Arguments).
