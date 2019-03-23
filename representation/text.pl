:- module(text, [
	replace_all/4, 
	join_strings/3,
	join_atoms/3
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