% -*- mode: prolog -*-

:- use_module(utils).
:- use_module(library(readutil)).
:- use_module(library(clpfd)).
:- use_module(library(yall)).
:- use_module(library(assoc)).

%%%

rotation(left - Num) :- number(Num).
rotation(right - Num) :- number(Num).

direction(Dir - _, Dir).

rotation_chars(left - Num, ['L' | Num_chars]) :- number_chars(Num, Num_chars).
rotation_chars(right - Num, ['R' | Num_chars]) :- number_chars(Num, Num_chars).

rotation_string(Rotation, String) :-
    ( var(String) ->
      rotation_chars(Rotation, Chars),
      string_chars(String, Chars)
    ; string_chars(String, Chars),
      rotation_chars(Rotation, Chars)
    ).

rotation_target(Pos1, right - Num, Pos2) :- Pos2 #= Pos1 + Num.
rotation_target(Pos1, left - Num, Pos2) :- Pos2 #= Pos1 - Num.

rotate(Pos1, Rotation, Pos2) :-
    rotation_target(Pos1, Rotation, Target),
    Pos2 #= Target mod 100.

run_sequence_part1([], _, Acc, Acc).
run_sequence_part1([Rotation | Rotations], Pos, Acc, Zeros) :-
    rotate(Pos, Rotation, Pos1),
    ( Pos1 = 0 ->
      Acc1 #= Acc + 1
    ; Acc1 = Acc
    ),
    run_sequence_part1(Rotations, Pos1, Acc1, Zeros).

read_input(File, Rotations) :-
    open(File, read, Stream),
    read_lines(Stream, Lines),
    maplist(rotation_string, Rotations, Lines).

solution_part1(File, Total) :-
    read_input(File, Rotations),
    run_sequence_part1(Rotations, 50, 0, Total).

%%%

run_sequence_part2([], _, Acc, Acc).
run_sequence_part2([Rotation | Rotations], Pos, Acc, Zeros) :-
    rotate(Pos, Rotation, Pos1),
    (  Rotation = left - Amount
    -> Target #= Pos - 100 + Amount
    ;  Rotation = right - Amount,
       Target #= Pos + Amount
    ),
    Num_rotations #= abs(Target div 100),
    Acc1 #= Acc + Num_rotations,
    run_sequence_part2(Rotations, Pos1, Acc1, Zeros).

solution_part2(File, Total) :-
    read_input(File, Rotations),
    run_sequence_part2(Rotations, 50, 0, Total).
