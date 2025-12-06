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

solution(Problems, Solution) :-
    maplist(run_operation, Problems, Results),
    sumlist(Results, Solution).

%%%

split_on_empty_lines([], Acc, Acc).
split_on_empty_lines(Input, Acc, Res) :-
    append(Chunk, [Space_line | Rest], Input),
    maplist(=(' '), Space_line),
    !,
    split_on_empty_lines(Rest, [Chunk | Acc], Res).

split_on_empty_lines(Input, Res) :-
    split_on_empty_lines(Input, [], Res).

chunk_operation([Hd | Tl], operation(Op, Nums)) :-
    reverse(Hd, [Op | _]),
    append(First_num, [Op], Hd),
    maplist(
        [Num_chars_padded, Num] >> (
            maplist(char_code, Num_chars_padded, Num_codes_padded),
            phrase((blanks, integer(Num), blanks), Num_codes_padded),
            !
        ),
        [First_num | Tl],
        Nums).

read_input_part2(File, Problems) :-
    open(File, read, Stream),
    read_lines(Stream, Line_strings),
    maplist(string_chars, Line_strings, Line_chars),
    maplist(
        ({Len}/[Line, Line_padded] >> (
            length(Line_padded, Len),
            append(Line, Padding, Line_padded),
            maplist(=(' '), Padding)
        )),
        Line_chars,
        Line_chars_padded
    ),
    transpose(Line_chars_padded, Transposed),
    split_on_empty_lines(Transposed, Chunks),
    maplist(chunk_operation, Chunks, Problems),
    !.
