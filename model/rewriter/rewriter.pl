:- module(rewriter, [
  rewrite_file/3,
  rewrite_linking_file/3
]).

:- use_module('../graph', [edge/3, vertex/2, find_facts/2]).
:- use_module('../../arrays', [filter/3]).
:- use_module('../../system/io', [write_lines/2]).
:- use_module('../../representation/text', [term_to_string/2, sort_strings/2]).

:- dynamic visited/1.

% Visiting Theorems
set_visited(Fact) :-
  assertz(visited(Fact)).

unset_visited(Fact) :-
  retract(Fact).

retract_visits :-
  retractall(visited(_)).

% Root Traversing Theorems
is_property_edge(edge(_, Label, _)) :-
  member(Label, [name, package]).

find_outgoing_property_edges(Head, PropertyEdges) :-
  find_outgoing_edges(Head, Edges),
  filter(Edges, rewriter:is_property_edge, PropertyEdges).

find_outgoing_edges(Head, Edges) :-
  findall(edge(Head, Label, Tail), edge(Head, Label, Tail), Edges).

collect_facts(_, [], _, _, []).
collect_facts(Root, [Fact|Rest], Roots, OtherRoots, Facts) :-
  collect_facts(Root, Fact, Roots, OtherRoots, PartialFacts),
  collect_facts(Root, Rest, Roots, OtherRoots, OtherFacts),
  append(PartialFacts, OtherFacts, Facts).
collect_facts(vertex(RootDescriptor, RootLabel), vertex(Descriptor, Label), Roots, _, []) :-
  vertex(RootDescriptor, RootLabel) \== vertex(Descriptor, Label),
  member(vertex(Descriptor, Label), Roots).
collect_facts(Root, vertex(Descriptor, Label), Roots, OtherRoots, [vertex(Descriptor, Label)|Rest]) :-
  \+ visited(vertex(Descriptor, Label)),
  member(vertex(Descriptor, Label), OtherRoots),
  set_visited(vertex(Descriptor, Label)),
  find_outgoing_property_edges(Label, Edges),
  collect_facts(Root, Edges, Roots, OtherRoots, Rest).
collect_facts(Root, vertex(Descriptor, Label), Roots, OtherRoots, [vertex(Descriptor, Label)|Rest]) :-
  \+ visited(vertex(Descriptor, Label)),
  set_visited(vertex(Descriptor, Label)),
  find_outgoing_edges(Label, Edges),
  collect_facts(Root, Edges, Roots, OtherRoots, Rest).
collect_facts(Root, edge(Head, Label, Tail), Roots, OtherRoots, [edge(Head, Label, Tail)|Rest]) :-
  \+ visited(edge(Head, Label, Tail)),
  vertex(Descriptor, Tail),
  set_visited(edge(Head, Label, Tail)),
  collect_facts(Root, vertex(Descriptor, Tail), Roots, OtherRoots, Rest).
collect_facts(_, edge(Head, Label, Tail), _, _, [edge(Head, Label, Tail)]) :-
  \+ visited(edge(Head, Label, Tail)),
  set_visited(edge(Head, Label, Tail)).
collect_facts(_, _, _, _, []).

collect_root_facts([], _, _, []).
collect_root_facts([Root|Rest], Roots, OtherRoots, Facts) :-
  collect_facts(Root, Root, Roots, OtherRoots, PartialFacts),
  collect_root_facts(Rest, Roots, OtherRoots, OtherFacts),
  append(PartialFacts, OtherFacts, Facts).

% Knowledge Base Traversing Theorems
collect_knowledge_base(File, Roots, AllRoots, KnowledgeBase) :-
  subtract(AllRoots, Roots, OtherRoots),
  collect_root_facts(Roots, Roots, OtherRoots, RootFacts),
  find_facts(File, SourceFacts),
  union(RootFacts, SourceFacts, KnowledgeBase).

%% Knowledge Base Writing
% Converting Theorems
knowledge_base_to_strings([], []).
knowledge_base_to_strings([Fact|FactsRest], [String|StringsRest]) :-
  term_to_string(Fact, String),
  knowledge_base_to_strings(FactsRest, StringsRest).

% Writing Theorems
start_writing(File, Stream) :-
  open(File, write, Stream, [create([default])]).

finish_writing(Stream) :-
  close(Stream).

write_knowledge_base(File, KnowledgeBase) :-
  knowledge_base_to_strings(KnowledgeBase, KnowledgeBaseStrings),
  sort_strings(KnowledgeBaseStrings, SortedKnowledgeBaseStrings),
  start_writing(File, Stream),
  write_lines(Stream, SortedKnowledgeBaseStrings),
  finish_writing(Stream).

% Rewriting Theorems
rewrite_file(File, Roots, AllRoots) :-
  collect_knowledge_base(File, Roots, AllRoots, KnowledgeBase),
  write_knowledge_base(File, KnowledgeBase),
  retract_visits.

rewrite_linking_file(File, Repository, Uses) :-
  write_knowledge_base(File, [Repository|Uses]).
