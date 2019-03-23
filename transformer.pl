:- use_module('model/model').

print_all([]).
print_all([X|Y]) :-
	write(X), nl,
	print_all(Y).

start :-
	load_graph("D:/Workspace/MitrasTest/Graph.pl"),
	find_class_by_name("Employee", Employee),
	find_type_by_name("String", Type),
	add_attribute(Employee, ['private'], Type, "preferredFruit", _), !,
	add_method(Employee, ['public'], void, "kill", [
		parameter([], 'String', person),
		parameter([], 'boolean', old)
	], _),
	findall(X, edge(X, unsynchronized, X), Unsynchronized),
	findall(param(N, O), (vertex(parameter, N), edge(N, unsynchronized, N), edge(N, order, O)), UnsyncParams),
	print_all(UnsyncParams),
	print_all(Unsynchronized).