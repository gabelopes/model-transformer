:- use_module('model/model').
:- use_module('system/console', [write_all/1]).
:- use_module('integration/java', [invoke_java/2, inject_attribute/5, inject_getter/4, inject_setter/4]).
:- use_module('cli', [find_option/3]).

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

start2 :-
  invoke_java('/Users/sap/Workspace/Repositories/HelloWorld/out/artifacts/HelloWorld_jar/HelloWorld.jar', ["Jeff"]).

start1 :-
  load_graph("/Users/sap/Workspace/Repositories/AGG-Test/graph.pl"),
  testA,
  synchronize_graph.

start3 :-
  inject_attribute("/Users/sap/Downloads/Employee.java", "Employee", ["protected"], "int", "age"),
  inject_getter("/Users/sap/Downloads/Employee.java", "Employee", ["protected"], "age"),
  inject_setter("/Users/sap/Downloads/Employee.java", "Employee", ["protected"], "age").

start :-
  Options = [c(d), a(b)],
  find_option(Options, a, Value),
  write(Value).
