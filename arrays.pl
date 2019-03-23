:- module(arrays, [
	has_multiple_occurrences/2,
	has_multiple_occurrences_del/2,
	has_multiple_occurrences_sel/2,
	occurrences/3,
	occurrences_del/3
]).

has_multiple_occurrences_sel(Element, Array) :-
	select(Element, Array, Differential),
	member(Element, Differential).

has_multiple_occurrences(Element, Array) :-
	occurrences(Element, Array, Occurrences),
	Occurrences > 1.

has_multiple_occurrences_del(Element, Array) :-
	occurrences_del(Element, Array, Occurrences),
	Occurrences > 1.

occurrences(_, [], Count, Count).
occurrences(Element, [Head|Tail], Count, Occurrences) :-
	Element \= Head,
	occurrences(Element, Tail, Count, Occurrences).
occurrences(Element, [_|Tail], Count, Occurrences) :-
	NextCount is Count + 1,
	occurrences(Element, Tail, NextCount, Occurrences).
occurrences(Element, Array, Occurrences) :-
	occurrences(Element, Array, 0, Occurrences), !.

occurrences_del(Element, Array, Occurrences) :-
	delete(Array, Element, Differential),
	length(Array, X),
	length(Differential, Y),
	Occurrences is X - Y.