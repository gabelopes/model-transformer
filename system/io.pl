:- module(io, [
  escape_argument/2,
  escape_arguments/2,
  read_stream_to_lines/2,
  writef/2,
  writef/3,
  writefe/2,
  writefe/3,
  write_fact/1,
  write_fact/2,
  write_lines/1,
  write_lines/2
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

read_stream_to_lines(Stream, [Line|Rest]) :-
  read_line_to_string(Stream, Line),
  Line \= end_of_file,
  read_stream_to_lines(Stream, Rest).
read_stream_to_lines(_, []).

writef(Stream, Format, Arguments) :-
  format(atom(FormattedString), Format, Arguments),
  write(Stream, FormattedString).

writefe(Stream, Format, Arguments) :-
  escape_arguments(Arguments, EscapedArguments),
  writef(Stream, Format, EscapedArguments).

writef(Format, Arguments) :-
  current_output(OutputStream),
  writef(OutputStream, Format, Arguments).

writefe(Format, Arguments) :-
  current_output(OutputStream),
  writefe(OutputStream, Format, Arguments).

write_fact(Stream, edge(Head, Label, Tail)) :-
  writefe(Stream, 'edge(~w, ~w, ~w).', [Head, Label, Tail]).
write_fact(Stream, vertex(Descriptor, Label)) :-
  writefe(Stream, 'vertex(~w, ~w).', [Descriptor, Label]).
write_fact(Stream, Fact) :-
  portray_clause(Stream, Fact).

write_fact(Fact) :-
  current_output(OutputStream),
  write_fact(OutputStream, Fact).

write_lines(_, []).
write_lines(Stream, [String|Rest]) :-
  writeln(Stream, String),
  write_lines(Stream, Rest).

write_lines(Strings) :-
  current_output(OutputStream),
  write_lines(OutputStream, Strings).
