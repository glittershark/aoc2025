% -*- mode: prolog -*-

:- use_module(utils).
:- use_module(library(readutil)).
:- use_module(library(assoc)).
:- use_module(library(yall)).
:- use_module(library(record)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(clpfd)).
:- use_module(library(simplex), []).

%%%

:- record machine(goal_state, buttons, joltages).

binary_decimal([], 0).
binary_decimal([Bit | Bits], Res) :-
    Bit in 0 \/ 1,
    length(Bits, Position),
    binary_decimal(Bits, Next),
    Res #= Bit * (2 ^ Position) + Next.

%%%
%%% Parsing
%%%

%%% All light states are encoded as binary numbers.
%%%
%%% Buttons are also binary numbers, with "pressing a button" represented as
%%% taking the current state of the lights XOR with the button

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

press_buttons(Buttons, State0, State1) :-
    foldl(press_button, Buttons, State0, State1).

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

%%%
%%% Part 2
%%%

press_button_joltage(Bits, Joltages0, Joltages1) :-
    maplist([Bit, Joltage0, Joltage1] >> (Joltage1 #= Joltage0 + Bit),
            Bits,
            Joltages0,
            Joltages1).

press_buttons_joltage(Buttons, Joltage0, Joltage1) :-
    foldl(press_button_joltage, Buttons, Joltage0, Joltage1).

machine_joltage_sequence(Machine, Sequence) :-
    machine_buttons(Machine, Buttons),
    machine_joltages(Machine, Target_joltages),

    length(Target_joltages, Machine_len),
    maplist(
        {Machine_len} / [Bits, Button] >> (
            binary_decimal(Bits, Button),
            length(Bits, Machine_len)
        ),
        Bits_buttons,
        Buttons),
    !,

    same_length(Target_joltages, Joltages0),
    maplist(=(0), Joltages0),

    foldl(
        {Bits_buttons, Target_joltages} / [Bits, Joltages0, Joltages1] >> (
            member(Bits, Bits_buttons),
            press_button_joltage(Bits, Joltages0, Joltages1),
            maplist(#=<, Joltages1, Target_joltages)
        ),
        Sequence,
        Joltages0,
        Target_joltages
    ).

shortest_joltage_sequence_len(Machine, Len) :-
    machine_buttons(Machine, Buttons),
    machine_joltages(Machine, Target_joltages),
    length(Target_joltages, Machine_len),
    maplist(
        {Machine_len} / [Bits, Button] >> (
            binary_decimal(Bits, Button),
            length(Bits, Machine_len)
        ),
        Bits_buttons,
        Buttons),
    !,

    findall(button(I), nth0(I, Buttons, _), Button_vars),
    findall(I, nth0(I, Target_joltages, _), Joltage_indexes),

    simplex:gen_state(State0),
    foldl(
        [Button_var, State_0, State_1] >>
        ( simplex:constraint(integral(Button_var), State_0, State_1) ),
        Button_vars,
        State0,
        State1
    ),
    foldl(
        {Bits_buttons, Button_vars} / [Joltage, Joltage_index, State_0, State_1] >> (
            maplist(
                {Joltage_index} / [Button, Var, Expr] >>
                ( nth0(Joltage_index, Button, Button_joltage),
                  Expr = Button_joltage * Var
                ),
                Bits_buttons,
                Button_vars,
                Exprs
            ),
            simplex:constraint(Exprs = Joltage, State_0, State_1)
        ),
        Target_joltages,
        Joltage_indexes,
        State1,
        State2),

    maplist([Button_var, 1 * Button_var] >> true, Button_vars, Objective),
    !,
    simplex:minimize(Objective, State2, State3),
    simplex:objective(State3, Len).

solution_part2(Machines, Solution) :-
    maplist(shortest_joltage_sequence_len, Machines, Sequence_lens),
    sumlist(Sequence_lens, Solution).
