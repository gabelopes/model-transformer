:- module(console, [
  write_all/1,
  write_all/2
]).

write_all(Terms) :-
  write_all(Terms, "\n").

write_all([], _).
write_all([Term|Rest], Separator) :-
  write(Term),
  write(Separator),
  write_all(Rest, Separator).
