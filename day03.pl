% -*- mode: prolog -*-

:- use_module(utils).
:- use_module(library(readutil)).

%%%

battery_pair_joltage(Battery1 - Battery2, Joltage) :-
    Joltage is (Battery1 * 10) + Battery2.

largest_joltage(Bank, Joltage) :-
    append(Except_last, [_], Bank),
    max_member(Battery1, Except_last),
    append(_, [Battery1 | After_battery1], Bank),
    max_member(Battery2, After_battery1),
    !,
    battery_pair_joltage(Battery1 - Battery2, Joltage).

digit_code(Digit, Code) :- Digit is Code - 48.

string_bank(String, Bank) :-
    string_codes(String, Codes),
    maplist(digit_code, Bank, Codes).

read_input(Input, Banks) :-
    open(Input, read, Stream),
    read_lines(Stream, Lines),
    maplist(string_bank, Lines, Banks).

solution_part1(Banks, Total_joltage) :-
    maplist(largest_joltage, Banks, Joltages),
    sum_list(Joltages, Total_joltage).
