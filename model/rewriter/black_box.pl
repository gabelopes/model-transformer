:- module(black_box, [
  load_black_box/1,
  unload_black_box/0,
  get_orphan_facts/1
]).

:- use_module('../graph', [is_root/1]).

% Loading Theorems
load_black_box(File) :-
  exists_file(File),
  load_files([File], [register(false)]).
load_black_box(_).

unload_black_box :-
  abolish(black_box:edge/3),
  abolish(black_box:vertex/2).

% Oprhan Vertices Theorems
is_orphan(vertex(Descriptor, Label)) :-
  \+ is_root(vertex(Descriptor, Label)),
  \+ edge(_, _, Label).

get_orphan_edges([], []).
get_orphan_edges([vertex(_, VertexLabel)|Rest], Edges) :-
  findall(edge(VertexLabel, Label, Tail), edge(VertexLabel, Label, Tail), VertexEdges),
  get_orphan_edges(Rest, Partial),
  append(VertexEdges, Partial, Edges).

get_orphan_vertices(Vertices) :-
  findall(vertex(Descriptor, Label), (
    vertex(Descriptor, Label),
    is_orphan(vertex(Descriptor, Label))
  ), Vertices).

get_orphan_facts(Facts) :-
  get_orphan_vertices(Vertices),
  get_orphan_edges(Vertices, Edges),
  append(Vertices, Edges, Facts).
