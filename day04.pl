% -*- mode: prolog -*-

:- use_module(utils).
:- use_module(library(readutil)).
:- use_module(library(clpfd)).

%%%

read_input(File, Matrix) :-
    open(File, read, Stream),
    read_lines(Stream, Lines),
    maplist(string_chars, Lines, Matrix).

%%%

at_pos(X - Y, Matrix, V) :-
    nth0(X, Matrix, Row),
    nth0(Y, Row, V).

adjacent(X1 - Y1, X2 - Y2) :-
    X2 #>= X1 - 1, X2 #=< X1 + 1,
    Y2 #>= Y1 - 1, Y2 #=< Y1 + 1,
    ( X2 #\= X1 ; Y2 #\= Y1 ).

paper_at_pos(X - Y, Matrix) :-
    at_pos(X - Y, Matrix, '@').

rolls_adjacent(Pos, Matrix, Rolls) :-
    setof(
        X - Y,
        (adjacent(Pos, X -  Y),
         indomain(X),
         indomain(Y),
         paper_at_pos(X - Y, Matrix)),
        Ps),
    length(Ps, Rolls).

forklift_can_pick_up(Pos, Matrix) :-
    paper_at_pos(Pos, Matrix),
    ( rolls_adjacent(Pos, Matrix, Rolls),
      Rolls #< 4
    ; \+ rolls_adjacent(Pos, Matrix, _)
    ).

pickupable_positions(Matrix, Positions) :-
    setof(Pos, forklift_can_pick_up(Pos, Matrix), Positions).
pickupable_positions(Matrix, []) :-
    \+ forklift_can_pick_up(_, Matrix).

solution_part1(Matrix, Solution) :-
    pickupable_positions(Matrix, Positions),
    length(Positions, Solution).

%%%

remove_at_pos(X - Y, Matrix1, Matrix2) :-
    replace0(Row1, Y, _, '.', Row2),
    replace0(Matrix1, X, Row1, Row2, Matrix2).

single_step(Matrix1, Matrix2, Removed) :-
    pickupable_positions(Matrix1, Removed),
    foldl(remove_at_pos, Removed, Matrix1, Matrix2).

run_step(Matrix, Total, Total) :- single_step(Matrix, Matrix, []), !.
run_step(Matrix1, Acc, Total) :-
    single_step(Matrix1, Matrix2, Removed),
    length(Removed, N),
    Acc1 is Acc + N,
    run_step(Matrix2, Acc1, Total).

solution_part2(Matrix, Solution) :-
    run_step(Matrix, 0, Solution).
