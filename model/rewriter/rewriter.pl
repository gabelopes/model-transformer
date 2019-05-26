:- module(rewriter, [
  rewrite_file/3,
  rewrite_linking_file/3
]).

:- use_module('../graph', [edge/3, vertex/2, is_root/1]).
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

% Oprhan Vertices Theorems
is_orphan(vertex(Descriptor, Label)) :-
  \+ is_root(vertex(Descriptor, Label)),
  \+ edge(_, _, Label).

collect_orphan_edges([], []).
collect_orphan_edges([vertex(_, VertexLabel)|Rest], Edges) :-
  findall(edge(VertexLabel, Label, Tail), edge(VertexLabel, Label, Tail), VertexEdges),
  collect_orphan_edges(Rest, Partial),
  append(VertexEdges, Partial, Edges).

collect_orphan_vertices(Vertices) :-
  findall(vertex(Descriptor, Label), (
    vertex(Descriptor, Label),
    is_orphan(vertex(Descriptor, Label))
  ), Vertices).

collect_orphan_facts(Facts) :-
  collect_orphan_vertices(Vertices),
  collect_orphan_edges(Vertices, Edges),
  append(Vertices, Edges, Facts).

collect_knowledge_base(FileRoots, Roots, KnowledgeBase) :-
  subtract(Roots, FileRoots, OtherFilesRoots),
  collect_all_facts(FileRoots, FileRoots, OtherFilesRoots, Facts),
  collect_orphan_facts(Orphans),
  append(Facts, Orphans, KnowledgeBase).

% Comparative Theorems
fact_to_string(Fact, String) :-
  with_output_to(string(String), portray_clause(Fact)).

compare_facts(Delta, FactA, FactB) :-
  fact_to_string(FactA, StringA),
  fact_to_string(FactB, StringB),
  compare(Delta, StringA, StringB).

sort_knowledge_base(KnowledgeBase, Sorted) :-
  predsort(compare_facts, KnowledgeBase, Sorted).

% Writing Theorems
write_knowledge_base(_, []).
write_knowledge_base(Stream, [Fact|Rest]) :-
  portray_clause(Stream, Fact),
  write_knowledge_base(Stream, Rest).

start_writing(File, Stream) :-
  open(File, write, Stream, [create([default])]).

finish_writing(Stream) :-
  close(Stream).

% Rewriting Theorems
rewrite_file(File, FileRoots, Roots) :-
  collect_knowledge_base(FileRoots, Roots, KnowledgeBase),
  sort_knowledge_base(KnowledgeBase, SortedKnowledgeBase),
  start_writing(File, Stream),
  write_knowledge_base(Stream, SortedKnowledgeBase),
  finish_writing(Stream),
  retract_visits.

rewrite_linking_file(File, Repository, Uses) :-
  sort_knowledge_base(Uses, SortedUses),
  start_writing(File, Stream),
  write_knowledge_base(Stream, [Repository|SortedUses]),
  finish_writing(Stream).
