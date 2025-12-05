% -*- mode: prolog -*-

:- use_module(utils).
:- use_module(library(readutil)).

%%%

digit_code(Digit, Code) :- Digit is Code - 48.

string_bank(String, Bank) :-
    string_codes(String, Codes),
    maplist(digit_code, Bank, Codes).

read_input(Input, Banks) :-
    open(Input, read, Stream),
    read_lines(Stream, Lines),
    maplist(string_bank, Lines, Banks).

%%%

battery_pair_joltage(Battery1 - Battery2, Joltage) :-
    Joltage is (Battery1 * 10) + Battery2.

largest_pair_joltage(Bank, Joltage) :-
    append(Except_last, [_], Bank),
    max_member(Battery1, Except_last),
    append(_, [Battery1 | After_battery1], Bank),
    max_member(Battery2, After_battery1),
    !,
    battery_pair_joltage(Battery1 - Battery2, Joltage).

solution_part1(Banks, Total_joltage) :-
    maplist(largest_pair_joltage, Banks, Joltages),
    sum_list(Joltages, Total_joltage).

%%%

largest_n_joltage(_, 0, 0).
largest_n_joltage(Bank, N, Joltage) :-
    Exclude is N - 1,
    append(Except_excluded, Excluded, Bank),
    length(Excluded, Exclude),
    max_member(Battery1, Except_excluded),
    !,
    N1 is N - 1,
    append(_, [Battery1 | After_battery1], Bank),
    largest_n_joltage(After_battery1, N1, Joltage0),
    Joltage is Battery1 * (10 ** (N - 1)) + Joltage0.

largest_12_joltage(Bank, Joltage) :-
    largest_n_joltage(Bank, 12, Joltage).

solution_part2(Banks, Total_joltage) :-
    maplist(largest_12_joltage, Banks, Joltages),
    sum_list(Joltages, Total_joltage).
