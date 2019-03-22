:- module(graph, [
	load_graph/1,
	unload_graph/0,
	edge/3,
	vertex/2,
	create_edge/3,
	create_vertex/2
]).

:- reexport(entity).
:- reexport(interface).
:- reexport(class).
:- reexport(attribute).
:- reexport(method).
:- reexport(parameter).

:- multifile edge/3.
:- multifile vertex/2.

:- dynamic loaded/1.
:- dynamic use/1.
:- dynamic link/2.
:- dynamic edge/3.
:- dynamic vertex/2.

% Graph Theorems
create_edge(Head, Label, Tail) :-
	assertz(edge(Head, Label, Tail)).

create_vertex(Descriptor, Label) :-
	assertz(vertex(Descriptor, Label)).

find_classes(Classes) :-
	findall(Class, vertex(class, Class), Classes).

is_branch_class(Class) :-
	vertex(class, Class),
	edge(Class, Label, _),
	Label \= 'name',
	Label \= 'package'.

retract_graph :-
	retractall(edge(_, _, _)),
	retractall(vertex(_, _)).

% Links Theorems
create_link(File, Class) :-
	is_branch_class(Class),
	assertz(link(File, Class)).
create_link(_, _).

create_links(_, []).
create_links(File, [Class|Rest]) :-
	create_link(File, Class),
	create_links(File, Rest).

create_links([]).
create_links([Use|Rest]) :-
	consult(Use),
	find_classes(Classes),
	create_links(Use, Classes),
	retract_graph,
	create_links(Rest).

retract_links :-
	retractall(link(_)).

% Uses Theorems
find_uses(Uses) :-
	findall(Use, use(Use), Uses).

retract_uses :-
	retractall(use(_)).

% Module Theorems
retract_all :-
	retract_graph,
	retract_links,
	retract_uses.

% Consulting Theorems
consult_all(Uses) :-
	maplist(consult, Uses).

% Loading Theorems
loaded(false).

set_loaded(Bool) :-
	retract(loaded(_)),
	assertz(loaded(Bool)).

load_graph(File) :-
	loaded(false),
	consult(File),
	find_uses(Uses),
	create_links(Uses),
	consult_all(Uses),
	retract_uses,
	set_loaded(true).

unload_graph :-
	retract_all,
	set_loaded(false).