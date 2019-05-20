:- module(arrays, [
  has_multiple_occurrences/2,
  occurrences/3,
  filter/3
]).

has_multiple_occurrences(Element, Array) :-
  occurrences(Element, Array, Occurrences),
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

filter([], _, []).
filter([Element|ArrayRest], Predicate, [Element|FilteredRest]) :-
  call(Predicate, Element),
  filter(ArrayRest, Predicate, FilteredRest).
filter([_|ArrayRest], Predicate, FilteredRest) :-
  filter(ArrayRest, Predicate, FilteredRest).
