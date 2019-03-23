:- use_module('model/model').

start :-
	load_graph("D:/Workspace/MitrasTest/Graph.pl"),
	find_class_by_name("Employee", Employee),
	find_type_by_name("String", Type),
	add_attribute(Employee, "CPF", Type, Attribute), !,
	add_method(Employee, "kill", void, ['public'], [
		parameter([final], 'String', person),
		parameter([], 'boolean', old)
	]),
	edge(Attribute, unsynchronized, Attribute),
	write(Attribute).