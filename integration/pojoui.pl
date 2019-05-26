:- module(pojoui, [
  inject_panel/2,
  inject_panel/3,
  inject_panel/4,
  inject_panel/5,
  inject_panel_label/3,
  inject_panel_visibility/3,
  inject_panel_position/3,
  inject_panel_deletion/2,
  inject_field/3,
  inject_field/4,
  inject_field/5,
  inject_field/6,
  inject_field_label/4,
  inject_field_visibility/4,
  inject_field_position/4,
  inject_field_deletion/3
]).

:- use_module(library(http/json)).
:- use_module(injector).
:- use_module('../representation/text', [atom_to_number/2]).

get_language("pojo-ui").

get_extension_path(Path) :-
  getenv('PARTHENOS_JAVA_POJOUI_INJECTOR_JAR', Path).

% Injector
invoke_injector(File, Injection, JSON) :-
  get_extension_path(Extension),
  get_language(Language),
  invoke_injector(Extension, File, Language, Injection, JSON).

%% Injections
% Panel Injections
inject_panel(File, Class) :-
  atom_json_dict(JSON, _{ class: Class }, []),
  invoke_injector(File, "create-panel", JSON).
inject_panel(File, Class, Label) :-
  atom_json_dict(JSON, _{ class: Class, label: Label }, []),
  invoke_injector(File, "create-panel", JSON).
inject_panel(File, Class, Label, Visibility) :-
  atom_json_dict(JSON, _{ class: Class, label: Label, visible: Visibility }, []),
  invoke_injector(File, "create-panel", JSON).
inject_panel(File, Class, Label, Visibility, Position) :-
  atom_to_number(Position, PositionNumber),
  atom_json_dict(JSON, _{ class: Class, label: Label, visible: Visibility, position: PositionNumber }, []),
  invoke_injector(File, "create-panel", JSON).

inject_panel_label(File, Class, Label) :-
  atom_json_dict(JSON, _{ class: Class, label: Label }, []),
  invoke_injector(File, "set-panel-label", JSON).

inject_panel_visibility(File, Class, Visibility) :-
  atom_json_dict(JSON, _{ class: Class, visible: Visibility }, []),
  invoke_injector(File, "set-panel-visibility", JSON).

inject_panel_position(File, Class, Position) :-
  atom_to_number(Position, PositionNumber),
  atom_json_dict(JSON, _{ class: Class, position: PositionNumber }, []),
  invoke_injector(File, "set-panel-position", JSON).

inject_panel_deletion(File, Class) :-
  atom_json_dict(JSON, _{ class: Class }, []),
  invoke_injector(File, "remove-panel", JSON).

% Field Injections
inject_field(File, Class, Attribute) :-
  atom_json_dict(JSON, _{ class: Class, attribute: Attribute }, []),
  invoke_injector(File, "create-field", JSON).
inject_field(File, Class, Attribute, Label) :-
  atom_json_dict(JSON, _{ class: Class, attribute: Attribute, label: Label }, []),
  invoke_injector(File, "create-field", JSON).
inject_field(File, Class, Attribute, Label, Visibility) :-
  atom_json_dict(JSON, _{ class: Class, attribute: Attribute, label: Label, visible: Visibility }, []),
  invoke_injector(File, "create-field", JSON).
inject_field(File, Class, Attribute, Label, Visibility, Position) :-
  atom_to_number(Position, PositionNumber),
  atom_json_dict(JSON, _{ class: Class, attribute: Attribute, label: Label, visible: Visibility, position: PositionNumber }, []),
  invoke_injector(File, "create-field", JSON).

inject_field_label(File, Class, Attribute, Label) :-
  atom_json_dict(JSON, _{ class: Class, attribute: Attribute, label: Label }, []),
  invoke_injector(File, "set-field-label", JSON).

inject_field_visibility(File, Class, Attribute, Visibility) :-
  atom_json_dict(JSON, _{ class: Class, attribute: Attribute, visible: Visibility }, []),
  invoke_injector(File, "set-field-visibility", JSON).

inject_field_position(File, Class, Attribute, Position) :-
  atom_to_number(Position, PositionNumber),
  atom_json_dict(JSON, _{ class: Class, attribute: Attribute, position: PositionNumber }, []),
  invoke_injector(File, "set-field-position", JSON).

inject_field_deletion(File, Class, Attribute) :-
  atom_json_dict(JSON, _{ class: Class, attribute: Attribute }, []),
  invoke_injector(File, "remove-field", JSON).
