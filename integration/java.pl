:- module(java, [
  invoke_java/2,
  invoke_java/3,
  inject_attribute/5,
  inject_getter/4,
  inject_setter/4
]).

:- use_module(library(process)).
:- use_module(library(http/json)).
:- use_module('../system/console', [write_all/1]).
:- use_module('../representation/text', [join_strings/3, replace_all/4, atoms_strings/2]).

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

% Injector Theorems
get_injector_executable_path(Path) :-
  getenv('INJECTOR_JAR', Path).

invoke_injector(File, Injection, JSON) :-
  get_injector_executable_path(Injector),
  invoke_java(Injector, ["-s", File, "-l", "JAVA", "-i", Injection, JSON]).

inject_attribute(File, Class, Modifiers, Type, Name) :-
  atom_json_dict(JSON, _{ class: Class, modifiers: Modifiers, type: Type, name: Name }, []),
  invoke_injector(File, "attribute", JSON).

inject_getter(File, Class, Modifiers, Attribute) :-
  atom_json_dict(JSON, _{ class: Class, modifiers: Modifiers, attribute: Attribute }, []),
  invoke_injector(File, "getter", JSON).

inject_setter(File, Class, Modifiers, Attribute) :-
  atom_json_dict(JSON, _{ class: Class, modifiers: Modifiers, attribute: Attribute }, []),
  invoke_injector(File, "setter", JSON).
