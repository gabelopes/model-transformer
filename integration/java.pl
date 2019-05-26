:- module(java, [
  inject_attribute/5,
  inject_getter/4,
  inject_setter/4,
  inject_class/7
]).

:- use_module(library(http/json)).
:- use_module(injector).
:- use_module('../arrays', [get_last_element/2]).

get_language("java").

get_extension_path(Path) :-
  getenv('PARTHENOS_JAVA_INJECTOR_JAR', Path).

% Integrations
invoke_injector_with_output(File, Injection, JSON, Output) :-
  get_extension_path(Extension),
  get_language(Language),
  invoke_injector_with_output(Extension, File, Language, Injection, JSON, Output).

invoke_injector(File, Injection, JSON) :-
  get_extension_path(Extension),
  get_language(Language),
  invoke_injector(Extension, File, Language, Injection, JSON).

inject_attribute(File, Class, Modifiers, Type, Name) :-
  atom_json_dict(JSON, _{ class: Class, modifiers: Modifiers, type: Type, name: Name }, []),
  invoke_injector(File, "attribute", JSON).

inject_getter(File, Class, Modifiers, Attribute) :-
  atom_json_dict(JSON, _{ class: Class, modifiers: Modifiers, attribute: Attribute }, []),
  invoke_injector(File, "getter", JSON).

inject_setter(File, Class, Modifiers, Attribute) :-
  atom_json_dict(JSON, _{ class: Class, modifiers: Modifiers, attribute: Attribute }, []),
  invoke_injector(File, "setter", JSON).

inject_class(Folder, Package, Modifiers, Name, Parent, Interfaces, SourceFile) :-
  atom_json_dict(JSON, _{ package: Package, modifiers: Modifiers, name: Name, parent: Parent, interfaces: Interfaces }, []),
  invoke_injector_with_output(Folder, "create-class", JSON, Output),
  get_last_element(Output, Last),
  atom_string(SourceFile, Last).
