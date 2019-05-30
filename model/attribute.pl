:- module(attribute, [
  is_attribute/1,
  find_attribute_by_name/3,
  get_attribute_class/2,
  get_attribute_modifiers/2,
  get_attribute_type/2,
  get_attribute_name/2,
  add_attribute/5,
  create_accessors/2
]).

:- use_module(graph, [edge/3, vertex/2, create_edge/3, create_vertex/2]).
:- use_module(common, [is_type/1, get_name/2, get_type/2, get_modifiers/2]).
:- use_module(class, [is_class/1, find_class/2]).
:- use_module(method, [is_method/1, add_method/6]).
:- use_module(modifier, [is_modifier/1]).
:- use_module('../representation/qualified_name').
:- use_module('../representation/text', [capitalize/2]).

% Assertion Theorems
is_attribute(Attribute) :-
  nonvar(Attribute),
  vertex(attribute, Attribute),
  edge(_, attribute, Attribute).

% Search Theorems
find_attribute_by_name(Class, Name, Attribute) :-
  find_class(Class, ClassLabel),
  edge(Attribute, name, Name),
  vertex(attribute, Attribute),
  edge(ClassLabel, attribute, Attribute).

% Properties Theorems
get_attribute_class(Attribute, Class) :-
  edge(Class, attribute, Attribute),
  is_class(Class).

get_attribute_modifiers(Attribute, Modifiers) :-
  is_attribute(Attribute),
  get_modifiers(Attribute, Modifiers).

get_attribute_type(Attribute, Type) :-
  is_attribute(Attribute),
  get_type(Attribute, Type).

get_attribute_name(Attribute, Name) :-
  is_attribute(Attribute),
  get_name(Attribute, Name).

%% Transformations
% Validation Theorems
modifiers_are_valid([]).
modifiers_are_valid([Modifier|Rest]) :-
  is_modifier(Modifier),
  modifiers_are_valid(Rest).

attribute_exists(Class, Name) :-
  edge(Class, attribute, Attribute),
  edge(Attribute, name, Name).

can_add_attribute(Class, Modifiers, Type, Name) :-
  is_class(Class),
  is_type(Type),
  modifiers_are_valid(Modifiers), !,
  \+ attribute_exists(Class, Name).

% Creation Theorems
create_modifiers_edges(_, []).
create_modifiers_edges(Attribute, [Modifier|Rest]) :-
  create_edge(Attribute, modifier, Modifier),
  create_modifiers_edges(Attribute, Rest).

create_getter(Class, Attribute) :-
  get_attribute_name(Attribute, Name),
  get_attribute_type(Attribute, Type),
  capitalize(Name, Capitalized),
  string_concat("get", Capitalized, MethodName),
  add_method(Class, ['public'], Type, MethodName, [], _).

create_setter(Class, Attribute) :-
  get_attribute_name(Attribute, Name),
  get_attribute_type(Attribute, Type),
  capitalize(Name, Capitalized),
  string_concat("set", Capitalized, MethodName),
  add_method(Class, ['public'], void, MethodName, [parameter([], Type, Name)], _).

create_accessors(Class, Attribute) :-
  create_getter(Class, Attribute),
  create_setter(Class, Attribute).

add_attribute(Class, Modifiers, Type, Name, Attribute) :-
  atom_string(Name, NameString),
  can_add_attribute(Class, Modifiers, Type, NameString),
  generate_qualified_name([Class, Name], Attribute),
  create_vertex(attribute, Attribute),
  create_edge(Class, attribute, Attribute),
  create_modifiers_edges(Attribute, Modifiers),
  create_edge(Attribute, type, Type),
  create_edge(Attribute, name, NameString).
