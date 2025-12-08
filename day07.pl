% -*- mode: prolog -*-

:- use_module(utils).
:- use_module(library(readutil)).
:- use_module(library(yall)).
:- use_module(library(clpfd)).
:- use_module(library(aggregate)).
:- use_module(library(dcg/basics)).
:- use_module(library(ordsets)).

%%%

read_input(File, Diagram) :-
    open(File, read, Stream),
    read_lines(Stream, Lines),
    maplist(string_chars, Lines, Diagram).

%%%

will_be_split(Y, Beams, Row) :-
    member(Y, Beams),
    nth0(Y, Row, '^').

beam_project(Beams, Row, Next_beams, Splits) :-
    aggregate_all(set(Y), Y, (member(Y, Beams), \+ nth0(Y, Row, '^')), Unsplit_beams),
    aggregate_all(
        set(Y),
        Y,
        ((Y1 #= Y + 1 ; Y1 #= Y - 1),
         will_be_split(Y1, Beams, Row)
        ),
        Split_beams
    ),
    aggregate_all(count, Y, (will_be_split(Y, Beams, Row)), Splits),
    append(Unsplit_beams, Split_beams, Next_beams_unsorted),
    sort(Next_beams_unsorted, Next_beams).

count_splits([], _, Acc, Acc).
count_splits([Row | Rows], Beams, Acc, Res) :-
    beam_project(Beams, Row, Next_beams, Splits),
    Acc1 is Acc + Splits,
    count_splits(Rows, Next_beams, Acc1, Res).

solution_part1([Source_row | Rows], Solution) :-
    nth0(Y, Source_row, 'S'),
    Beams = [Y],
    count_splits(Rows, Beams, 0, Solution).

solution_part1(Solution) :-
    read_input("day07.input", Diagram),
    solution_part1(Diagram, Solution).

%%%

list_multiset(List, Multiset) :-
    false.

merge_multisets(Set1, Set2, Set) :-
    false.

beam_project_part2(Beams, Row, Next_beams) :-
    aggregate_all(
        bag(Y),
        (
            get_assoc(Y, _, Beams),
            \+ nth0(Y, Row, '^')
        ),
        Unsplit_beams
    ),

    aggregate_all(
        bag(Y),
        (
            (Y1 #= Y + 1 ; Y1 #= Y - 1),
            will_be_split(Y1, Beams, Row)
        ),
        Split_beams
    ),

    append(Unsplit_beams, Split_beams, Next_beams).

count_timelines([], Beams, Res) :-
    length(Beams, Res).
count_timelines([Row | Rows], Beams, Res) :-
    beam_project_part2(Beams, Row, Next_beams),
    count_timelines(Rows, Next_beams, Res).

solution_part2([Source_row | Rows], Solution) :-
    nth0(Y, Source_row, 'S'),
    empty_assoc(Empty),
    put_assoc(Y, Empty, 1, Beams),
    count_timelines(Rows, Beams, Solution).

solution_part2(Solution) :-
    read_input("day07.input", Diagram),
    solution_part2(Diagram, Solution).

sample_part2(Solution) :-
    read_input("day07.sample", Diagram),
    solution_part2(Diagram, Solution).
