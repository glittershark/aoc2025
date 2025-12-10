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

num_split_beams(Y - Count, Beams, Row) :-
    gen_assoc(Y, Beams, Count),
    nth0(Y, Row, '^').

pairs_multiset(Pairs, Assoc) :-
    empty_assoc(Empty),
    foldl(
        multiset_add,
        Pairs,
        Empty,
        Assoc
    ).

multiset_add(Key - Value, Assoc1, Assoc2) :-
    ( get_assoc(Key, Assoc1, Current_value), !
    ; Current_value = 0
    ),
    New_value #= Current_value + Value,
    put_assoc(Key, Assoc1, New_value, Assoc2).

list_multiset(List, Multiset) :-
    maplist(
        [E, E - 1] >> true,
        List,
        Pairs
    ),
    empty_assoc(Empty),
    foldl(
        multiset_add,
        Pairs,
        Empty,
        Multiset
    ).

merge_multisets(Set1, Set2, Set) :-
    assoc_to_list(Set2, Pairs),
    foldl(
        multiset_add,
        Pairs,
        Set1,
        Set
    ).

multiset_cardinality(Assoc, Cardinality) :-
    assoc_to_list(Assoc, Pairs),
    maplist(
        [_ - V, V] >> true,
        Pairs,
        Counts
    ),
    sum_list(Counts, Cardinality).

beam_project_part2(Beams, Row, Next_beams) :-
    aggregate_all(
        bag(Y - Count),
        (
            gen_assoc(Y, Beams, Count),
            \+ nth0(Y, Row, '^')
        ),
        Unsplit_beams
    ),

    aggregate_all(
        bag(Y - Count),
        (
            (Y1 #= Y + 1 ; Y1 #= Y - 1),
            num_split_beams(Y1 - Count, Beams, Row)
        ),
        Split_beams
    ),

    pairs_multiset(Unsplit_beams, Unsplit_ms),
    pairs_multiset(Split_beams, Split_ms),
    merge_multisets(Unsplit_ms, Split_ms, Next_beams).

count_timelines([], Beams, Res) :-
    multiset_cardinality(Beams, Res).
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
