:- module(graph, [
  load_graph/1,
  unload_graph/0,
  edge/3,
  vertex/2,
  create_edge/3,
  replace_edge/2,
  remove_edge/3,
  create_vertex/2,
  replace_vertex/2,
  remove_vertex/2,
  create_root/2,
  is_root/1,
  create_use/1,
  create_use_for_name/2,
  find_source/2,
  rewrite_graph/0
]).

:- use_module(rewriter/rewriter, [rewrite_file/3, rewrite_linking_file/2]).
:- use_module('../arrays', [filter/3]).
:- use_module('../representation/qualified_name', [generate_file_name/3]).

:- multifile edge/3.
:- multifile vertex/2.

:- dynamic loaded/1.
:- dynamic use/1.
:- dynamic root/2.
:- dynamic edge/3.
:- dynamic vertex/2.

% Graph Theorems
create_edge(Head, Label, Tail) :-
  assertz(edge(Head, Label, Tail)).

replace_edge(edge(Head, Label, Tail), edge(ForHead, ForLabel, ForTail)) :-
  remove_edge(Head, Label, Tail),
  create_edge(ForHead, ForLabel, ForTail).

remove_edge(Head, Label, Tail) :-
  retract(edge(Head, Label, Tail)).
remove_edge(_, _, _).

create_vertex(Descriptor, Label) :-
  assertz(vertex(Descriptor, Label)).

replace_vertex(vertex(Descriptor, Label), vertex(ForDescriptor, ForLabel)) :-
  remove_vertex(Descriptor, Label),
  create_vertex(ForDescriptor, ForLabel).

remove_vertex(Descriptor, Label) :-
  retract(vertex(Descriptor, Label)).
remove_vertex(_, _).

retract_graph :-
  retractall(edge(_, _, _)),
  retractall(vertex(_, _)).

% Links Theorems
create_root(Term, File) :-
  assertz(root(Term, File)).
create_root(_, _).

create_roots([], _).
create_roots([Term|Rest], File) :-
  create_root(Term, File),
  create_roots(Rest, File).

create_roots([]).
create_roots([Use|Rest]) :-
  consult(Use),
  find_root_vertices(Vertices),
  create_roots(Vertices, Use),
  retract_graph,
  create_roots(Rest).

find_root_vertices(Roots) :-
  findall(vertex(Descriptor, Label), vertex(Descriptor, Label), Vertices),
  filter(Vertices, graph:is_root, Roots).

is_root(vertex(class, Class)) :-
  edge(Class, Label, _),
  \+ member(Label, [name, package]).

retract_roots :-
  retractall(root(_, _)).

% Uses Theorems
create_use(File) :-
  assertz(use(File)).

create_use_for_name(Name, Use) :-
  loaded(LinkingFile),
  generate_file_name(LinkingFile, Name, FileName),
  atom_concat(FileName, '.pl', Use),
  create_use(Use).

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
  set_loaded(File), !.

unload_graph :-
  retract_all,
  set_loaded(false).

% Searching Theorems
find_root(vertex(Descriptor, Label), File) :-
  root(vertex(Descriptor, Label), File).
find_root(vertex(Descriptor, Label), File) :-
  \+ is_root(Descriptor, Label),
  edge(ParentLabel, _, Label),
  ParentLabel \= Label,
  vertex(ParentDescriptor, ParentLabel),
  find_root(vertex(ParentDescriptor, ParentLabel), File).

find_source(vertex(_, Label), File) :-
  edge(Label, source, File).
find_source(vertex(Descriptor, Label), File) :-
  \+ is_root(vertex(Descriptor, Label)),
  edge(ParentLabel, _, Label),
  ParentLabel \= Label,
  vertex(ParentDescriptor, ParentLabel),
  find_source(vertex(ParentDescriptor, ParentLabel), File).

find_file_roots(File, Roots) :-
  findall(Term, root(Term, File), Roots).

find_all_roots(Roots) :-
  findall(Term, root(Term, _), Roots).

% Rewriting Theorems
rewrite_files([]).
rewrite_files([Use|Rest]) :-
  find_file_roots(Use, FileRoots),
  find_all_roots(Roots),
  rewrite_file(Use, FileRoots, Roots),
  rewrite_files(Rest).

rewrite_graph :-
  find_uses(Uses),
  rewrite_files(Uses),
  loaded(LinkingFile),
  rewrite_linking_file(LinkingFile, Uses).
