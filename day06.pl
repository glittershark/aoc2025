% -*- mode: prolog -*-

:- use_module(utils).
:- use_module(library(readutil)).
:- use_module(library(yall)).
:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).

%%%

run_operation(operation('*', Nums), Result) :-
    foldl([X, Y, Res] >> (Res is X * Y), Nums, 1, Result).
run_operation(operation('+', Nums), Result) :-
    foldl([X, Y, Res] >> (Res is X + Y), Nums, 0, Result).

line(numbers([])) --> blanks, eos, !.
line(numbers([N | Ns])) -->
    blanks,
    integer(N),
    !,
    line(numbers(Ns)).

line(operations([])) --> blanks, eos, !.
line(operations([Op | Ops])) -->
    blanks,
    [Op_code],
    { char_code(Op, Op_code),
      member(Op, ['*', '+'])
    },
    line(operations(Ops)).

line_string_lines(Line_strings, Lines) :-
    maplist([S, L] >> string_phrase(line(L), S), Line_strings, Lines).

read_input(File, Problems) :-
    open(File, read, Stream),
    read_lines(Stream, Line_strings),
    line_string_lines(Line_strings, Lines),
    reverse(Lines, [operations(Ops) | Nums]),
    maplist(([N, Ns] >> (N = numbers(Ns))), Nums, Numbers_transposed),
    transpose(Numbers_transposed, Numbers),
    !,
    maplist(
        ([Op, Nums, Problem] >> (Problem = operation(Op, Nums))),
        Ops,
        Numbers,
        Problems
    ).

solution_part1(Problems, Solution) :-
    maplist(run_operation, Problems, Results),
    sumlist(Results, Solution).
