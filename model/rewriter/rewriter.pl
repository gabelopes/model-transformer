:- module(rewriter, [
  rewrite_file/3
]).

:- use_module('../graph', [edge/3, vertex/2]).
:- use_module('../../system/io', [writefe/3]).
:- use_module('../../arrays', [filter/3]).

:- dynamic visited/1.

% Visiting Theorems
set_visited(Fact) :-
  assertz(visited(Fact)).

unset_visited(Fact) :-
  retract(Fact).

retract_visits :-
  retractall(visited(_)).

% Graph Walking Theorems
is_property_edge(edge(_, _, Tail)) :-
  \+ vertex(_, Tail).

find_outgoing_property_edges(Head, PropertyEdges) :-
  find_outgoing_edges(Head, Edges),
  filter(Edges, rewriter:is_property_edge, PropertyEdges).

find_outgoing_edges(Head, Edges) :-
  findall(edge(Head, Label, Tail), edge(Head, Label, Tail), Edges).

are_vertices_equal(vertex(Descriptor, Label), vertex(Descriptor, Label)).

collect_facts(_, [], _, _, []).
collect_facts(FileRoot, [Fact|Rest], FileRoots, OtherFilesRoots, Facts) :-
  collect_facts(FileRoot, Fact, FileRoots, OtherFilesRoots, PartialFacts),
  collect_facts(FileRoot, Rest, FileRoots, OtherFilesRoots, OtherFacts),
  append(PartialFacts, OtherFacts, Facts).
collect_facts(vertex(FileRootDescriptor, FileRootLabel), vertex(Descriptor, Label), FileRoots, _, []) :-
  \+ are_vertices_equal(vertex(FileRootDescriptor, FileRootLabel), vertex(Descriptor, Label)),
  member(vertex(Descriptor, Label), FileRoots).
collect_facts(FileRoot, vertex(Descriptor, Label), FileRoots, OtherFilesRoots, [vertex(Descriptor, Label)|Rest]) :-
  \+ visited(vertex(Descriptor, Label)),
  member(vertex(Descriptor, Label), OtherFilesRoots),
  set_visited(vertex(Descriptor, Label)),
  find_outgoing_property_edges(Label, Edges),
  collect_facts(FileRoot, Edges, FileRoots, OtherFilesRoots, Rest).
collect_facts(FileRoot, vertex(Descriptor, Label), FileRoots, OtherFilesRoots, [vertex(Descriptor, Label)|Rest]) :-
  \+ visited(vertex(Descriptor, Label)),
  set_visited(vertex(Descriptor, Label)),
  find_outgoing_edges(Label, Edges),
  collect_facts(FileRoot, Edges, FileRoots, OtherFilesRoots, Rest).
collect_facts(FileRoot, edge(Head, Label, Tail), FileRoots, OtherFilesRoots, [edge(Head, Label, Tail)|Rest]) :-
  \+ visited(edge(Head, Label, Tail)),
  vertex(Descriptor, Tail),
  set_visited(edge(Head, Label, Tail)),
  collect_facts(FileRoot, vertex(Descriptor, Tail), FileRoots, OtherFilesRoots, Rest).
collect_facts(_, edge(Head, Label, Tail), _, _, [edge(Head, Label, Tail)]) :-
  \+ visited(edge(Head, Label, Tail)),
  set_visited(edge(Head, Label, Tail)).
collect_facts(_, _, _, _, []).

collect_all_facts([], _, _, []).
collect_all_facts([FileRoot|Rest], FileRoots, OtherFilesRoots, Facts) :-
  collect_facts(FileRoot, FileRoot, FileRoots, OtherFilesRoots, PartialFacts),
  collect_all_facts(Rest, FileRoots, OtherFilesRoots, OtherFacts),
  append(PartialFacts, OtherFacts, Facts).

% Writing Theorems
write_fact(Stream, vertex(Descriptor, Label)) :-
  writefe(Stream, "vertex(~w, ~w).", [Descriptor, Label]),
  nl(Stream).
write_fact(Stream, edge(Head, Label, Tail)) :-
  writefe(Stream, "edge(~w, ~w, ~w).", [Head, Label, Tail]),
  nl(Stream).

write_facts(_, []).
write_facts(Stream, [Fact|Rest]) :-
  write_fact(Stream, Fact),
  write_facts(Stream, Rest).

start_writing(File, Stream) :-
  open(File, write, Stream).

finish_writing(Stream) :-
  close(Stream).

% Rewriting Theorems
rewrite_file(File, FileRoots, Roots) :-
  subtract(Roots, FileRoots, OtherFilesRoots),
  collect_all_facts(FileRoots, FileRoots, OtherFilesRoots, Facts),
  start_writing(File, Stream),
  write_facts(Stream, Facts),
  finish_writing(Stream),
  retract_visits.
