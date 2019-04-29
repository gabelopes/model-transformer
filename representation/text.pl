:- module(text, [
  replace_all/4,
  atom_replace_all/4,
  join_strings/3,
  join_atoms/3,
  capitalize/2,
  atoms_strings/2
]).

join_strings(Initial, [], _, Initial).
join_strings(Initial, [Head|Tail], Delimiter, Joined) :-
  string_concat(Initial, Delimiter, X),
  join_strings(Head, Tail, Delimiter, Y),
  string_concat(X, Y, Joined).

join_strings([Head|Tail], Delimiter, Joined) :-
  join_strings(Head, Tail, Delimiter, Joined).

join_atoms(Initial, [], _, Initial).
join_atoms(Initial, [Head|Tail], Delimiter, Joined) :-
  atom_concat(Initial, Delimiter, X),
  join_atoms(Head, Tail, Delimiter, Y),
  atom_concat(X, Y, Joined).

join_atoms([Head|Tail], Delimiter, Joined) :-
  join_atoms(Head, Tail, Delimiter, Joined).

replace_all(String, Old, New, Replaced) :-
  split_string(String, Old, "", Parts),
  join_strings(Parts, New, Replaced).

atom_replace_all(Atom, Old, New, Replaced) :-
  atom_string(Atom, String),
  replace_all(String, Old, New, ReplacedString),
  atom_string(Replaced, ReplacedString).

capitalize(Text, Capitalized) :-
  atom(Text),
  atom_chars(Text, [FirstChar|Rest]),
  upcase_atom(FirstChar, CapitalizedFirstChar),
  atom_chars(Capitalized, [CapitalizedFirstChar|Rest]).
capitalize(Text, Capitalized) :-
  string(Text),
  string_chars(Text, [FirstChar|Rest]),
  upcase_atom(FirstChar, CapitalizedFirstChar),
  string_chars(Capitalized, [CapitalizedFirstChar|Rest]).

atoms_strings([], []).
atoms_strings([Atom|Atoms], [String|Strings]) :-
  atom_string(Atom, String),
  atoms_strings(Atoms, Strings).
