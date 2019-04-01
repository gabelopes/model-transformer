:- use_module('model/model').
:- use_module('system/console', [write_all/1]).
:- use_module('integration/java', [invoke_java/2]).

testA :-
  find_class_by_name("Employee", Employee),
  find_type_by_name("String", Type),
  add_attribute(Employee, ['private'], Type, "preferredFruit", _), !,
  add_method(Employee, ['public'], void, "kill", [
    parameter([], 'String', person),
    parameter([], 'boolean', old)
  ], _), !,
  find_class_by_name("Person", Person),
  add_attribute(Person, ['public'], 'String', "CPF", _), !,
  findall(X, edge(X, unsynchronized, X), Unsynchronized),
  findall(param(N, O), (vertex(parameter, N), edge(N, unsynchronized, N), edge(N, order, O)), UnsyncParams),
  write_all(UnsyncParams),
  write_all(Unsynchronized).

testB :-
  find_class_by_name("Employee", Employee),
  find_method_by_name(Employee, "tryLuck", Method),
  get_method_parameters(Method, Parameters),
  get_method_parameters_sorted(Method, SortedParameters),
  write_all(Parameters),
  write_all(SortedParameters).

testC :-
  find_class_by_name("Employee", Employee),
  add_method(Employee, ['public'], void, "sayHello", [
    parameter([], 'String', person),
    parameter([], 'int', times)
  ], Method),
  add_method(Employee, ['public'], void, "sayHello", [
    parameter([], 'String', person)
  %	parameter([], 'int', times)
  ], Method2),
  print(Method),
  print(Method2).

start :-
  invoke_java('/Users/sap/Workspace/Projects/HelloWorld/out/artifacts/HelloWorld_jar/HelloWorld.jar', ["Yo"]).
  %load_graph("/Users/sap/Workspace/Repositories/AGG-Test/graph.pl"),
  %testA,
  %synchronize_graph.
