:- module(graph, [
  repository/1,
  edge/3,
  vertex/2,
  create_edge/3,
  replace_edge/2,
  remove_edge/3,
  create_vertex/2,
  replace_vertex/2,
  remove_vertex/2,
  create_root/2,
  create_use/1,
  create_use/2,
  load_graph/1,
  unload_graph/0,
  find_source/2,
  find_edges/2,
  find_vertices/2,
  find_facts/2,
  rewrite_graph/0
]).

:- use_module(rewriter/rewriter, [rewrite_file/3, rewrite_linking_file/3]).
:- use_module('../arrays', [filter/3]).
:- use_module('../representation/qualified_name', [generate_file_name/3]).

:- dynamic loaded/1.
:- dynamic repository/1.
:- dynamic use/1.
:- dynamic root/2.
:- dynamic source_edge/4.
:- dynamic source_vertex/3.

% Graph Theorems
edge(Head, Label, Tail) :-
  source_edge(Head, Label, Tail, _).

create_edge(Head, Label, Tail) :-
  assertz(source_edge(Head, Label, Tail, null)).

replace_edge(edge(Head, Label, Tail), edge(ForHead, ForLabel, ForTail)) :-
  remove_edge(Head, Label, Tail),
  create_edge(ForHead, ForLabel, ForTail).

remove_edge(Head, Label, Tail) :-
  retractall(source_edge(Head, Label, Tail, _)).
remove_edge(_, _, _).

vertex(Descriptor, Label) :-
  source_vertex(Descriptor, Label, _).

create_vertex(Descriptor, Label) :-
  assertz(source_vertex(Descriptor, Label, null)).

replace_vertex(vertex(Descriptor, Label), vertex(ForDescriptor, ForLabel)) :-
  remove_vertex(Descriptor, Label),
  create_vertex(ForDescriptor, ForLabel).

remove_vertex(Descriptor, Label) :-
  retractall(source_vertex(Descriptor, Label, _)).
remove_vertex(_, _).

% Root Theorems
create_root(Fact, Source) :-
  assertz(root(Fact, Source)).
create_root(_, _).

% Source Theorems
create_source_graph([], _).
create_source_graph([root(Fact)|Rest], Source) :-
  create_root(Fact, Source),
  create_source_graph(Rest, Source).
create_source_graph([vertex(Descriptor, Label)|Rest], Source) :-
  assertz(source_vertex(Descriptor, Label, Source)),
  create_source_graph(Rest, Source).
create_source_graph([edge(Head, Label, Tail)|Rest], Source) :-
  assertz(source_edge(Head, Label, Tail, Source)),
  create_source_graph(Rest, Source).
create_source_graph([_|Rest], Source) :-
  create_source_graph(Rest, Source).

retract_source_graph :-
  retractall(source_edge(_, _, _, _)),
  retractall(source_vertex(_, _, _)),
  retractall(root(_, _)).

% Uses Theorems
create_use(File) :-
  assertz(use(File)).

create_use(Name, Use) :-
  loaded(LinkingFile),
  generate_file_name(LinkingFile, Name, FileName),
  atom_concat(FileName, '.pl', Use),
  create_use(Use).

find_uses(Uses) :-
  findall(use(Use), use(Use), Uses).

retract_uses :-
  retractall(use(_)).

% Module Theorems
retract_all :-
  retract_source_graph,
  retract_uses.

% Graph Creating Theorems
create_graph([]).
create_graph([use(Use)|Rest]) :-
  read_file_to_terms(Use, Terms, []),
  create_source_graph(Terms, Use),
  create_graph(Rest).

% Loading Theorems
loaded(false).

set_loaded(File) :-
  retractall(loaded(_)),
  assertz(loaded(File)).

load_graph(File) :-
  loaded(false),
  consult(File),
  find_uses(Uses),
  create_graph(Uses),
  set_loaded(File), !.

unload_graph :-
  retract_all,
  set_loaded(false).

% Searching Theorems
find_source(vertex(_, Label), Source) :-
  edge(Label, source, Source).
find_source(vertex(Descriptor, Label), Source) :-
  \+ root(vertex(Descriptor, Label), _),
  edge(ParentLabel, _, Label),
  ParentLabel \== Label,
  vertex(ParentDescriptor, ParentLabel),
  find_source(vertex(ParentDescriptor, ParentLabel), Source).

find_roots(Source, Roots) :-
  findall(Fact, root(Fact, Source), Roots).

find_roots(Roots) :-
  findall(Fact, root(Fact, _), Roots).

find_edges(Source, Edges) :-
  findall(edge(Head, Tail, Label), source_edge(Head, Tail, Label, Source), Edges).

find_vertices(Source, Vertices) :-
  findall(vertex(Descriptor, Label), source_vertex(Descriptor, Label, Source), Vertices).

find_facts(Source, [root(Root)|Facts]) :-
  find_edges(Source, Edges),
  find_vertices(Source, Vertices),
  root(Root, Source),
  union(Edges, Vertices, Facts).

% Rewriting Theorems
rewrite_files([]).
rewrite_files([use(Use)|Rest]) :-
  find_roots(Use, Roots),
  find_roots(AllRoots),
  rewrite_file(Use, Roots, AllRoots),
  rewrite_files(Rest).

rewrite_graph :-
  find_uses(Uses),
  rewrite_files(Uses),
  loaded(LinkingFile),
  repository(Repository),
  rewrite_linking_file(LinkingFile, repository(Repository), Uses).
