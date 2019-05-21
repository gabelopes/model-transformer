:- module(parameter, [
  find_parameter_by_name/4,
  get_parameter_modifiers/2,
  get_parameter_type/2,
  get_parameter_name/2,
  get_parameter_order/2,
  compare_parameters/3,
  sort_parameters/2,
  add_parameter/6
]).

:- use_module(graph, [edge/3, vertex/2, create_edge/3, create_vertex/2]).
:- use_module(common, [is_type/1, get_name/2, get_type/2, get_modifiers/2]).
:- use_module(method, [is_method/1, find_method/3]).
:- use_module(modifier, [is_modifier/1]).
:- use_module('../representation/qualified_name', [generate_qualified_name/2]).

% Assertion Theorems
is_parameter(Parameter) :-
  vertex(parameter, Parameter),
  edge(_, parameter, Parameter).

% Search Theorems
find_parameter_by_name(Class, Method, Name, Parameter) :-
  find_method(Class, Method, MethodLabel),
  edge(Parameter, name, Name),
  vertex(parameter, Parameter),
  edge(MethodLabel, parameter, Parameter).

% Properties Theorems
get_parameter_modifiers(Parameter, Modifiers) :-
  is_parameter(Parameter),
  get_modifiers(Parameter, Modifiers).

get_parameter_type(Parameter, Type) :-
  is_parameter(Parameter),
  get_type(Parameter, Type).

get_parameter_name(Parameter, Name) :-
  is_parameter(Parameter),
  get_name(Parameter, Name).

get_parameter_order(Parameter, Order) :-
  is_parameter(Parameter),
  edge(Parameter, order, Order).

% Comparative Theorems
compare_orders(<, OrderA, OrderB) :-
  OrderA < OrderB.
compare_orders(>, OrderA, OrderB) :-
  OrderA > OrderB.
compare_orders(=, _, _).

compare_parameters(Delta, ParameterA, ParameterB) :-
  get_parameter_order(ParameterA, OrderA),
  get_parameter_order(ParameterB, OrderB),
  compare_orders(Delta, OrderA, OrderB).

% Sorting Theorems
sort_parameters(Parameters, Sorted) :-
  predsort(compare_parameters, Parameters, Sorted).

%% Transformation Theorems
% Validation Theorems
modifiers_are_valid([]).
modifiers_are_valid([Modifier|Rest]) :-
  is_modifier(Modifier),
  modifiers_are_valid(Rest).

can_add_parameter(Method, Modifiers, Type) :-
  is_method(Method),
  is_type(Type),
  modifiers_are_valid(Modifiers).

% Creation Theorems
create_modifiers_edges(_, []).
create_modifiers_edges(Parameter, [Modifier|Rest]) :-
  create_edge(Parameter, modifier, Modifier),
  create_modifiers_edges(Parameter, Rest).

add_parameter(Method, Modifiers, Type, Name, Order, Parameter) :-
  can_add_parameter(Method, Modifiers, Type),
  generate_qualified_name([Method, Name], Parameter),
  create_vertex(parameter, Parameter),
  create_edge(Method, parameter, Parameter),
  create_modifiers_edges(Parameter, Modifiers),
  create_edge(Parameter, type, Type),
  create_edge(Parameter, name, Name),
  create_edge(Parameter, order, Order).
