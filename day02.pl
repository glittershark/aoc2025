% -*- mode: prolog -*-

:- use_module(utils).
:- use_module(library(readutil)).
:- use_module(library(dcg/basics), [integer/3]).
:- use_module(library(dcg/high_order)).
:- use_module(library(lists), [clumped/2]).

%%%

id_range(Lo - Hi) -->
    integer(Lo),
    "-",
    integer(Hi).

id_ranges(Ranges) -->
    sequence(id_range, ",", Ranges),
    optional("\n", true).

read_input(File, Ranges) :-
    open(File, read, Stream),
    read_stream_to_codes(Stream, Codes),
    phrase(id_ranges(Ranges), Codes).

id_in_ranges(Id, Ranges) :-
    member(Lo - Hi, Ranges),
    between(Lo, Hi, Id).

%%%

invalid_id_part1(Id) :-
    number_chars(Id, Chars),
    append(Digits, Digits, Chars).

solution_part1(File, Solution) :-
    read_input(File, Ranges),
    setof(
        Id,
        ( id_in_ranges(Id, Ranges),
          invalid_id_part1(Id)
        ),
        Invalid_ids),
    sum_list(Invalid_ids, Solution).

%%%


is_concatenation(L, Part) :-
    Part = [_ | _],
    append(Part, Suffix, L),
    is_concatenation(Suffix, Part).
is_concatenation(L, Part) :-
    append(Part, Part, L).

invalid_id_part2(Id) :-
    number_chars(Id, Chars),
    is_concatenation(Chars, _).

solution_part2(File, Solution) :-
    read_input(File, Ranges),
    setof(
        Id,
        ( id_in_ranges(Id, Ranges),
          invalid_id_part2(Id)
        ),
        Invalid_ids),
    sum_list(Invalid_ids, Solution).
