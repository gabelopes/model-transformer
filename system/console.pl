:- module(console, [
  write_all/1
]).

write_all([]).
write_all([Term|Rest]) :-
  writeln(Term),
  write_all(Rest).
