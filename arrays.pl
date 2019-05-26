:- module(arrays, [
  has_multiple_occurrences/2,
  occurrences/3,
  filter/3,
  parse_array/2,
  get_last_element/2
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

get_last_element([], _) :- fail.
get_last_element([Last], Last).
get_last_element([_|Rest], Last) :-
  get_last_element(Rest, Last).

%% Array Parsing DCG
array([]) --> space.
array(Content) --> space, left_bracket, space, array_content(Content), space, right_bracket, space.

array_content([]) --> space.
array_content(Content) --> elements(Content).

elements([Element]) --> element(Element).
elements([Element|Rest]) --> element(Element), space, comma, space, elements(Rest).

element(Identifier) --> double_quote, identifier(Identifier), double_quote.

identifier('') --> [].
identifier(Identifier) --> identifier_content(Content), { atom_codes(Identifier, Content), ! }.

identifier_content([]), """" --> double_quote.
identifier_content([34|Rest]) --> "\\""", identifier_content(Rest).
identifier_content([Code|Rest]) --> [Code], identifier_content(Rest).

space --> [].
space --> " ", space.

left_bracket --> "[".
right_bracket --> "]".

comma --> ",".

double_quote --> """".

parse_array(Text, Array) :-
  string_codes(Text, Stream),
  phrase(array(Array), Stream).
