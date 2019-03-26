:- use_module('model/model').

print_all([]).
print_all([X|Y]) :-
	write(X), nl,
	print_all(Y).

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
	print_all(UnsyncParams),
	print_all(Unsynchronized).

testB :- 
	find_class_by_name("Employee", Employee),
	find_method_by_name(Employee, "tryLuck", Method),
	get_method_parameters(Method, Parameters),
	get_method_parameters_sorted(Method, SortedParameters),
	print_all(Parameters),
	print_all(SortedParameters).

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
	load_graph("C:\\Users\\i867761\\OneDrive - SAP SE\\Desktop\\AGG-Test\\AGG-Test\\graph.pl"),
	testA,
	synchronize_graph.