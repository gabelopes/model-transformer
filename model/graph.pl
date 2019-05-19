:- module(graph, [
  load_graph/1,
  unload_graph/0,
  edge/3,
  vertex/2,
  create_edge/3,
  create_vertex/2,
  synchronize_graph/0,
  find_owner/2,
  find_source/2
]).

:- use_module('../system/io', [writefe/3]).

:- multifile edge/3.
:- multifile vertex/2.

:- dynamic loaded/1.
:- dynamic use/1.
:- dynamic owner/2.
:- dynamic edge/3.
:- dynamic vertex/2.

% Graph Theorems
create_edge(Head, Label, Tail) :-
  assertz(edge(Head, Label, Tail)).

remove_edge(Head, Label, Tail) :-
  retract(edge(Head, Label, Tail)).

create_vertex(Descriptor, Label) :-
  assertz(vertex(Descriptor, Label)),
  mark_unsynchronized(Label, add).

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
create_owner(Class, File) :-
  is_branch_class(Class),
  assertz(owner(Class, File)).
create_owner(_, _).

create_owners([], _).
create_owners([Class|Rest], File) :-
  create_owner(Class, File),
  create_owners(Rest, File).

create_owners([]).
create_owners([Use|Rest]) :-
  consult(Use),
  find_classes(Classes),
  create_owners(Classes, Use),
  retract_graph,
  create_owners(Rest).

retract_owners :-
  retractall(owner(_, _)).

% Uses Theorems
find_uses(Uses) :-
  findall(Use, use(Use), Uses).

retract_uses :-
  retractall(use(_)).

% Module Theorems
retract_all :-
  retract_graph,
  retract_owners,
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
  create_owners(Uses),
  consult_all(Uses),
  retract_uses,
  set_loaded(true), !.

unload_graph :-
  retract_all,
  set_loaded(false).

% Writing Theorems
write_vertex(Stream, Descriptor, Label) :-
  writefe(Stream, "vertex(~w, ~w).", [Descriptor, Label]),
  nl(Stream).

write_edges(_, []).
write_edges(Stream, [edge(Head, Label, Tail)|Rest]) :-
  writefe(Stream, "edge(~w, ~w, ~w).", [Head, Label, Tail]),
  nl(Stream),
  write_edges(Stream, Rest).

start_writing(File, Stream) :-
  open(File, append, Stream),
  nl(Stream).

finish_writing(Stream) :-
  close(Stream).

% Synchronizing Theorems
mark_synchronized(Vertex) :-
  vertex(_, Vertex),
  remove_edge(Vertex, unsynchronized(_), Vertex).

is_synchronized(Label) :-
  \+ is_unsynchronized(Label).

is_synchronized :-
  \+ is_unsynchronized.

mark_unsynchronized(Vertex, Type) :-
  vertex(_, Vertex),
  create_edge(Vertex, unsynchronized(Type), Vertex).

is_unsynchronized(Label) :-
  edge(Label, unsynchronized(_), Label).

is_unsynchronized :-
  edge(_, unsynchronized(_), _).

find_owner(Label, File) :-
  owner(Label, File).
find_owner(Label, File) :-
  \+ is_branch_class(Label),
  edge(Parent, _, Label),
  Parent \= Label,
  vertex(_, Parent),
  find_owner(Parent, File).

find_source(Label, File) :-
  edge(Label, source, File).
find_source(Label, File) :-
  \+ is_branch_class(Label),
  edge(Parent, _, Label),
  Parent \= Label,
  vertex(_, Parent),
  find_source(Parent, File).

find_unsynchronized_vertices(Vertices) :-
  findall(vertex(Descriptor, Vertex), (
    edge(Vertex, unsynchronized(_), Vertex),
    vertex(Descriptor, Vertex)
  ), Vertices).

find_unsynchronized_incoming_edges(Vertex, Edges) :-
  findall(edge(Head, Label, Vertex), (
    edge(Head, Label, Vertex),
    Label \= unsynchronized(_),
    vertex(_, Head),
    is_synchronized(Head)
  ), Edges).

find_unsynchronized_outgoing_edges(Vertex, Edges) :-
  findall(edge(Vertex, Label, Tail), (
    edge(Vertex, Label, Tail),
    Label \= unsynchronized(_)
  ), Edges).

find_unsynchronized_edges(Vertex, Edges) :-
  find_unsynchronized_incoming_edges(Vertex, IncomingEdges),
  find_unsynchronized_outgoing_edges(Vertex, OutgoingEdges),
  append(IncomingEdges, OutgoingEdges, Edges).

synchronize_vertices([]).
synchronize_vertices([vertex(Descriptor, Label)|Rest]) :-
  find_owner(Label, File),
  start_writing(File, Stream),
  write_vertex(Stream, Descriptor, Label),
  find_unsynchronized_edges(Label, Edges),
  write_edges(Stream, Edges),
  finish_writing(Stream),
  synchronize_vertices(Rest).

mark_vertices_synchronized([]).
mark_vertices_synchronized([vertex(_, Label)|Rest]) :-
  mark_synchronized(Label),
  mark_vertices_synchronized(Rest).

synchronize_graph :-
  is_synchronized.
synchronize_graph :-
  find_unsynchronized_vertices(Vertices),
  synchronize_vertices(Vertices),
  mark_vertices_synchronized(Vertices).
