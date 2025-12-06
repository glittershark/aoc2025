% -*- mode: prolog -*-

:- use_module(utils).
:- use_module(library(readutil)).
:- use_module(library(clpfd)).
:- use_module(library(dcg/basics), [integer/3]).
:- use_module(library(aggregate)).

%%%

range(Lo-Hi) -->
    integer(Lo),
    "-",
    integer(Hi).

string_range(String, Range) :- string_phrase(range(Range), String).

read_input(File, database(Fresh_ranges, Available_ingredients)) :-
    open(File, read, Stream),
    read_lines(Stream, Lines),
    append(Ingredient_range_strings, ["" | Available_ingredient_strings], Lines),
    maplist(string_range, Ingredient_range_strings, Fresh_ranges),
    maplist(number_string, Available_ingredients, Available_ingredient_strings),
    !.

fresh_ingredient(database(Fresh_ranges, Available_ingredients), Ingredient) :-
    member(Ingredient, Available_ingredients),
    member(Lo-Hi, Fresh_ranges),
    between(Lo, Hi, Ingredient).

solution_part1(Db, Solution) :-
    setof(Ingredient, fresh_ingredient(Db, Ingredient), Fresh_ingredients),
    length(Fresh_ingredients, Solution).

%%%

fresh_ingredient_part2(
    database(Fresh_ranges, _),
    Ingredient
) :-
    member(Lo-Hi, Fresh_ranges),
    between(Lo, Hi, Ingredient).

solution_part2_slow(Db, Solution) :-
    setof(
        Ingredient,
        fresh_ingredient_part2(Db, Ingredient),
        Fresh_ingredients
    ),
    length(Fresh_ingredients, Solution).
