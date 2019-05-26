:- module(io, [
  escape_argument/2,
  escape_arguments/2,
  read_stream_to_lines/2
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
