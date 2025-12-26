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

%%%
%%% part 2
%%%

inside(Point, Rectangle) :-
    pos(X_pt, Y_pt) = Point,
    pos(X1, Y1) - pos(X2, Y2) = Rectangle,
    ( X1 < X2 -> X_pt > X1
    ; X2 < X1 -> X_pt > X2
    ; X1 = X2, X_pt = X1
    ),
    ( Y1 < Y2 -> Y_pt > Y1
    ; Y2 < Y1 -> Y_pt > Y2
    ; Y1 = Y2, Y_pt = Y1
    ).
