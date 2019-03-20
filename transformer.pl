:- use_module('graph/graph').

start :-
	load_graph("D:/Workspace/MitrasTest/a.gpl"),
	find_class_by_name("Employee", X),
	write(X).