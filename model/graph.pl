:- module(graph, [
  load_graph/1,
  unload_graph/0,
  edge/3,
  vertex/2,
  create_edge/3,
  create_vertex/2,
  rewrite_graph/0,
  find_source/3
]).

:- use_module(rewriter/rewriter, [rewrite_file/3]).

:- multifile edge/3.
:- multifile vertex/2.

:- dynamic loaded/1.
:- dynamic use/1.
:- dynamic root/3.
:- dynamic edge/3.
:- dynamic vertex/2.

% Graph Theorems
create_edge(Head, Label, Tail) :-
  assertz(edge(Head, Label, Tail)).

remove_edge(Head, Label, Tail) :-
  retract(edge(Head, Label, Tail)).

create_vertex(Descriptor, Label) :-
  assertz(vertex(Descriptor, Label)).

find_root_vertices(Vertices) :-
  findall(vertex(Descriptor, Label), is_root(Descriptor, Label), Vertices).

is_root(class, Class) :-
  vertex(class, Class), !,
  edge(Class, Label, _),
  Label \= 'name',
  Label \= 'package'.

retract_graph :-
  retractall(edge(_, _, _)),
  retractall(vertex(_, _)).

% Links Theorems
create_root(Descriptor, Label, File) :-
  assertz(root(Descriptor, Label, File)).
create_root(_, _, _).

create_roots([], _).
create_roots([vertex(Descriptor, Label)|Rest], File) :-
  create_root(Descriptor, Label, File),
  create_roots(Rest, File).

create_roots([]).
create_roots([Use|Rest]) :-
  consult(Use),
  find_root_vertices(Vertices),
  create_roots(Vertices, Use),
  retract_graph,
  create_roots(Rest).

retract_roots :-
  retractall(root(_, _, _)).

% Uses Theorems
find_uses(Uses) :-
  findall(Use, use(Use), Uses).

retract_uses :-
  retractall(use(_)).

% Module Theorems
retract_all :-
  retract_graph,
  retract_roots,
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
  create_roots(Uses),
  consult_all(Uses),
  set_loaded(true), !.

unload_graph :-
  retract_all,
  set_loaded(false).

% Searching Theorems
find_root(Descriptor, Label, File) :-
  root(Descriptor, Label, File).
find_root(Descriptor, Label, File) :-
  \+ is_root(Descriptor, Label),
  edge(ParentLabel, _, Label),
  ParentLabel \= Label,
  vertex(ParentDescriptor, ParentLabel),
  find_root(ParentDescriptor, ParentLabel, File).

find_source(_, Label, File) :-
  edge(Label, source, File).
find_source(Descriptor, Label, File) :-
  \+ is_root(Descriptor, Label),
  edge(ParentLabel, _, Label),
  ParentLabel \= Label,
  vertex(ParentDescriptor, ParentLabel),
  find_source(ParentDescriptor, ParentLabel, File).

find_file_roots(File, Roots) :-
  findall(vertex(Descriptor, Label), root(Descriptor, Label, File), Roots).

find_all_roots(Roots) :-
  findall(vertex(Descriptor, Label), root(Descriptor, Label, _), Roots).

% Rewriting Theorems
rewrite_files([]).
rewrite_files([Use|Rest]) :-
  find_file_roots(Use, FileRoots),
  find_all_roots(Roots),
  rewrite_file(Use, FileRoots, Roots),
  rewrite_files(Rest).

rewrite_graph :-
  find_uses(Uses),
  rewrite_files(Uses).
