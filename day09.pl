% -*- mode: prolog -*-

:- use_module(utils).
:- use_module(library(readutil)).
:- use_module(library(assoc)).
:- use_module(library(yall)).

%%%

read_input(File, Positions) :-
    open(File, read, Stream),
    read_lines(Stream, Lines),
    maplist([Line, pos(X, Y)] >> (
                split_string(Line, ",", "", Num_strings),
                maplist(number_string, [X, Y], Num_strings)
            ),
            Lines,
            Positions).

%%%

area(pos(X1, Y1) - pos(X2, Y2), Area) :-
    Area is (abs(X1 - X2) + 1) * (abs(Y1 - Y2) + 1).


solution_part1(Positions, Area) :-
    aggregate_all(
        max(Area, Corner1 - Corner2),
        ( select(Corner1, Positions, Positions1),
          member(Corner2, Positions1),
          area(Corner1 - Corner2, Area)
        ),
        max(Area, _)).

shape_string(Positions, Lines_chars) :-
    aggregate_all(max(X), member(pos(X, _), Positions), Max_x),
    aggregate_all(min(X), member(pos(X, _), Positions), Min_x),
    aggregate_all(max(Y), member(pos(_, Y), Positions), Max_y),
    aggregate_all(min(Y), member(pos(_, Y), Positions), Min_y),

    X_dim is Max_x - Min_x,
    Y_dim is Max_y - Min_y,

    length(Lines_chars, X_dim),
    maplist({Y_dim} / [Line] >> length(Line, Y_dim), Lines_chars),
    maplist({Lines_chars, X_min, Y_min} / [pos(X, Y)] >> (
                X_norm is X - X_min,
                Y_norm is Y - Y_min,

                nth1(X_norm, Lines_chars, Line),
                nth1(Y_norm, Line, '#')
            ),
            Positions),
    maplist(
        [Line] >> (
            maplist(
                [Char] >> (var(Char) -> Char = '.' ; true),
                Line
            )
        ),
        Lines_chars).
