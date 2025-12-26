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

segment_contains(A1 - B1, A2 - B2) :-
    ( A1 =< B1 ->
      ( A2 =< B2 ->
        A1 =< A2,
        B1 >= B2
      ; segment_contains(A1 - B1, B2 - A2)
      )
    ; segment_contains(B1 - A1, A2 - B2)
    ).

inside(pos(X_pt, Y_pt), Rectangle) :-
    pos(X1, Y1) - pos(X2, Y2) = Rectangle,
    ( X1 < X2 -> (X_pt > X1, X_pt < X2)
    ; X2 < X1 -> (X_pt > X2, X_pt < X1)
    ),
    ( Y1 < Y2 -> (Y_pt > Y1, Y_pt < Y2)
    ; Y2 < Y1 -> (Y_pt > Y2, Y_pt < Y1)
    ).

inside(Line_segment, Rectangle) :-
    pos(X1_l, Y1_l) - pos(X2_l, Y2_l) = Line_segment,
    pos(X1_r, Y1_r) - pos(X2_r, Y2_r) = Rectangle,
    ( inside(pos(X1_l, Y1_l), Rectangle)
    ; inside(pos(X2_l, Y2_l), Rectangle)
    ; X1_l = X2_l ->
      % does our constant coordinate lie within the rectangle?
      ( X1_l > X1_r,
        X1_l < X2_r
      ; X1_l < X1_r,
        X1_l > X2_r
      ),
      % do we actually pass through it?
      segment_contains(Y1_l - Y2_l, Y1_r - Y2_r)
    ; Y1_l = Y2_l ->
      % just flip the thing diagonally and make it the same problem lol
      inside(
          pos(Y1_l, X1_l) - pos(Y2_l, X2_l),
          pos(Y1_r, X1_r) - pos(Y2_r, X2_r)
      )
    ),
    !.

positions_segments(Positions, Segments) :-
    findall(
        P1 - P2,
        append(_, [P1, P2 | _], Positions),
        Segments_unlooped
    ),
    Positions = [Pos1 | _],
    last(Positions, Pos2),
    Segments = [Pos2 - Pos1 | Segments_unlooped].

solution_part2(Positions, Area) :-
    positions_segments(Positions, Segments),
    aggregate_all(
        max(Area, Corner1 - Corner2),
        ( select(Corner1, Positions, Positions1),
          member(Corner2, Positions1),
          \+ (member(Segment, Segments),
              inside(Segment, Corner1 - Corner2)
             ),
          area(Corner1 - Corner2, Area)
        ),
        max(Area, _)).
