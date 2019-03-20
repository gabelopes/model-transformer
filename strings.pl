:- module(strings, [replace_all/4, join_strings/3]).

join_strings(Initial, [], _, Initial).
join_strings(Initial, [Head|Tail], Delimiter, Joined) :-
	string_concat(Initial, Delimiter, X),
	join_strings(Head, Tail, Delimiter, Y),
	string_concat(X, Y, Joined).

join_strings([Head|Tail], Delimiter, Joined) :-
	join_strings(Head, Tail, Delimiter, Joined).

replace_all(String, Old, New, Replaced) :-
	split_string(String, Old, "", Parts),
	join_strings(Parts, New, Replaced).