:- module(injector, [
  invoke_injector_with_output/6,
  invoke_injector_with_output/5,
  invoke_injector/5,
  invoke_injector/4
]).

:- use_module(library(process)).
:- use_module('../system/console', [write_all/1]).
:- use_module('../representation/text', [join_strings/3, replace_all/4, atoms_strings/2]).
:- use_module('../system/io', [read_stream_to_lines/2]).

% Java Theorems
get_java_executable_path(Path) :-
  getenv('JAVA_HOME', Home),
  atom_concat(Home, '/bin/java', Path).

invoke_java(Jar, Arguments, Timeout) :-
  get_java_executable_path(Java),
  process_create(Java, ["-jar", Jar|Arguments], [
    stdin(std),
    stdout(std),
    stderr(std),
    process(PID),
    window(true)
  ]),
  process_wait(PID, _, [timeout(Timeout)]).
invoke_java(Jar, Arguments) :-
  invoke_java(Jar, Arguments, 120).

invoke_java_with_output(Jar, Arguments, Timeout, Output) :-
  get_java_executable_path(Java),
  process_create(Java, ["-jar", Jar|Arguments], [
    stdin(null),
    stdout(pipe(OutputStream)),
    stderr(null),
    process(PID),
    window(true)
  ]),
  process_wait(PID, _, [timeout(Timeout)]),
  read_stream_to_lines(OutputStream, Output).
invoke_java_with_output(Jar, Arguments, Output) :-
  invoke_java_with_output(Jar, Arguments, 120, Output).

% Injector Theorems
get_injector_executable_path(Path) :-
  getenv('PARTHENOS_INJECTOR_JAR', Path).

invoke_injector_with_output(Extension, File, Language, Injection, JSON, Output) :-
  get_injector_executable_path(Injector),
  invoke_java_with_output(Injector, ["-e", Extension, "-s", File, "-l", Language, "-i", Injection, JSON], Output).

invoke_injector_with_output(Extension, Language, Injection, JSON, Output) :-
  get_injector_executable_path(Injector),
  invoke_java_with_output(Injector, ["-e", Extension, "-l", Language, "-i", Injection, JSON], Output).

invoke_injector(Extension, File, Language, Injection, JSON) :-
  invoke_injector_with_output(Extension, File, Language, Injection, JSON).

invoke_injector(Extension, Language, Injection, JSON) :-
  invoke_injector_with_output(Extension, Language, Injection, JSON).
