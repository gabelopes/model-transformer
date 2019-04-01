:- module(java, [
  invoke_java/2,
  invoke_java/3
]).

:- use_module(library(process)).

get_java_executable_path(Path) :-
  getenv('JAVA_HOME', Home),
  atom_concat(Home, '/bin/java', Path).

invoke_java(Jar, Arguments, Timeout) :-
  get_java_executable_path(Java),
  process_create(Java, ["-jar", Jar|Arguments], [
    stdin(std),
    stdout(std),
    stderr(std),
    process(PID)
  ]),
  process_wait(PID, _, [timeout(Timeout)]).

invoke_java(Jar, Arguments) :-
  invoke_java(Jar, Arguments, 120).
