:- use_module('graph/graph').

start :-
	load_graph("D:/Workspace/MitrasTest/Graph.pl"),
	find_class_by_name("Employee", Employee),
	find_type_by_name("String", Type),
	add_attribute(Employee, "CPF", Type, Attribute),
	edge(X, unsynchronized, X),
	write(X).