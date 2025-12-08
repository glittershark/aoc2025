% -*- mode: prolog -*-

:- use_module(utils).
:- use_module(library(readutil)).
:- use_module(library(yall)).
:- use_module(library(clpfd)).
:- use_module(library(aggregate)).
:- use_module(library(dcg/basics)).

%%%

read_input(File, Diagram) :-
    open(File, read, Stream),
    read_lines(Stream, Lines),
    maplist(string_chars, Lines, Diagram).

%%%

width([Row0 | _ ], W) :-
    length(Row0, W).

at_pos(X - Y, Diagram, V) :-
    width(Diagram, Width),
    Y #>= 0, Y #< Width,
    nth0(X, Diagram, Row),
    nth0(Y, Row, V).

%% :- table start_position(_, _).
start_position(0 - Y, Diagram) :-
    at_pos(0 - Y, Diagram, 'S').

%% :- table splitter_at(_, ).
splitter_at(Pos, Diagram) :-
    at_pos(Pos, Diagram, '^').

beam_at(X - Y, Diagram) :-
    Prev #= X - 1,
    (Splitter_y #= Y - 1 ; Splitter_y #= Y + 1),
    splitter_at(X - Splitter_y, Diagram),
    beam_at(Prev - Splitter_y, Diagram).
beam_at(X - Y, Diagram) :-
    X #> 1,
    \+ splitter_at(X - Y, Diagram),
    Prev #= X - 1,
    beam_at(Prev - Y, Diagram).
beam_at(1 - Y, Diagram) :-
    start_position(0 - Y, Diagram).

solution_part1(Diagram, Solution) :-
    aggregate_all(
        count,
        X - Y,
        (
            splitter_at(X - Y, Diagram),
            Prev #= X - 1,
            beam_at(Prev - Y, Diagram)
        ),
        Solution
    ).
