:- module(black_box, [
  load/1,
  unload/0,
  get_orphan_vertices/1
]).

% Loading Theorems
load(File) :-
  load_files([File], [register(false)]).

unload :-
  abolish(edge/3),
  abolish(vertex/2).

% Oprhan Vertices Theorems
is_orphan(Label) :-
  \+ edge(Label, _, _),
  \+ edge(_, _, Label).

get_orphan_vertices(Vertices) :-
  findall(vertex(Descriptor, Label), (
    vertex(Descriptor, Label),
    is_orphan(Label)
  ), Vertices).
