% -*- mode: prolog -*-

:- use_module(utils).
:- use_module(library(readutil)).
:- use_module(library(assoc)).
:- use_module(library(yall)).
:- use_module(library(record)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(clpfd)).

%%%

:- record machine(goal_state, buttons, joltages).

%%%
%%% Parsing
%%%

%%% All light states are encoded as binary numbers.
%%%
%%% Buttons are also binary numbers, with "pressing a button" represented as
%%% taking the current state of the lights XOR with the button

binary_decimal([], 0).
binary_decimal([Bit | Bits], Res) :-
    length(Bits, Position),
    binary_decimal(Bits, Next),
    Res is Bit * (2 ^ Position) + Next.

goal_state_single(0) --> ".".
goal_state_single(1) --> "#".

goal_state(Len, Num) -->
    "[",
    sequence(goal_state_single, Bits_rev),
    { reverse(Bits_rev, Bits),
      length(Bits, Len),
      binary_decimal(Bits, Num)
    },
    "]".

button(Len, Num) -->
    "(",
    sequence(integer, ",", Bit_positions),
    ")",
    { Len_pred is Len - 1,
      numlist(0, Len_pred, Indexes),
      maplist(
          {Bit_positions} / [Index, Bit] >> (
              member(Index, Bit_positions) -> Bit = 1 ; Bit = 0
          ),
          Indexes,
          Bits_rev
      ),
      reverse(Bits_rev, Bits),
      binary_decimal(Bits, Num),
      !
    }.

buttons(Len, [Button]) -->
    " ", button(Len, Button).
buttons(Len, [Button | Buttons]) -->
    " ", button(Len, Button),
    buttons(Len, Buttons).

joltages(Joltages) -->
    "{", sequence(integer, ",", Joltages), "}".

machine(Machine) -->
    goal_state(Len, Goal_state),
    buttons(Len, Buttons),
    " ",
    joltages(Joltages),
    { make_machine([ goal_state(Goal_state),
                     buttons(Buttons),
                     joltages(Joltages)],
                   Machine),
      !
    }.

read_input(File, Machines) :-
    open(File, read, Stream),
    read_lines(Stream, Lines),
    maplist([Line, Machine] >> string_phrase(machine(Machine), Line),
            Lines, Machines).

%%%
%%% Part 1
%%%

press_button(Button, State0, State1) :-
    State1 is State0 xor Button.

press_buttons([], State, State).
press_buttons([Button | Buttons], State0, State_final) :-
    press_button(Button, State0, State1),
    press_buttons(Buttons, State1, State_final).

machine_button_sequence(Machine, Sequence) :-
    machine_buttons(Machine, Buttons),
    machine_goal_state(Machine, Goal_state),

    subseq(Buttons, Sequence, _),
    press_buttons(Sequence, 0, Goal_state).

shortest_button_sequence_len(Machine, Len) :-
    aggregate_all(
        min(Len, Sequence),
        (machine_button_sequence(Machine, Sequence),
         length(Sequence, Len)
        ),
        min(Len, _)).

solution_part1(Machines, Solution) :-
    maplist(shortest_button_sequence_len, Machines, Sequence_lens),
    sumlist(Sequence_lens, Solution).
