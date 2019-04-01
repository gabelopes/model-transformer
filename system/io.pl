:- module(io, [
  escape_argument/2,
  escape_arguments/2,
  writef/3,
  writefe/3
]).

escape_argument(Argument, Escaped) :-
  string(Argument),
  format(string(Escaped), '"~w"', [Argument]).
escape_argument(Argument, Escaped) :-
  atom(Argument),
  format(string(Escaped), "'~w'", [Argument]).
escape_argument(Argument, Argument).

escape_arguments([], []).
escape_arguments([Argument], [Escaped]) :-
  escape_argument(Argument, Escaped).
escape_arguments([Argument|Rest], [Escaped|EscapedRest]) :-
  escape_arguments(Rest, EscapedRest),
  escape_argument(Argument, Escaped).

writef(Stream, Format, Arguments) :-
  format(atom(FormattedString), Format, Arguments),
  write(Stream, FormattedString).

writefe(Stream, Format, Arguments) :-
  escape_arguments(Arguments, EscapedArguments),
  writef(Stream, Format, EscapedArguments).
