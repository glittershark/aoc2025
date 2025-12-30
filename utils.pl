% -*- mode: prolog -*-
:- module(utils, [read_lines/2,
                  binary_number/2,
                  times/5,
                  times/6,
                  string_phrase/2,
                  string_phrase/3,
                  phrase_file/2,
                  phrase_file/3,
                  lazy_sequence/5,

                  number_base_decimal/3,

                  chunks/3,
                  replace1/4,
                  replace0/5,
                  list_product/2,

                  empty_set/1,
                  set_add/3,
                  set_del/3,
                  set_member/2,
                  set_memberchk/2,
                  list_to_set/2,
                  set_to_list/2,
                  set_cardinality/2]).
:- use_module(library(clpfd)).
:- use_module(library(assoc)).

read_lines(Stream, []) :- at_end_of_stream(Stream).
read_lines(Stream, [X | L]) :-
    \+ at_end_of_stream(Stream),
    read_line_to_string(Stream, X),
    read_lines(Stream, L).

binary_number(Bs, N) :-
   binary_number_min(Bs, 0,N, N).

binary_number_min([], N,N, _M).
binary_number_min([B|Bs], N0,N, M) :-
   B in 0..1,
   N1 #= B+2*N0,
   M #>= N1,
   binary_number_min(Bs, N1,N, M).

times(N, G, L) --> times(N, G, [], L).

times(0, _, _, []) --> [].
times(1, G, _, [X]) -->
    call(G, X).
times(N, G, Sep, [X | Xs]) -->
    call(G, X),
    Sep,
    { N1 is N - 1 },
    times(N1, G, Sep, Xs).

:- meta_predicate
       string_phrase(//, ?),
       string_phrase(//, ?, ?).
string_phrase(RuleSet, InputString) :-
   ground(InputString),
   string_codes(InputString, Input),
   phrase(RuleSet, Input).
string_phrase(RuleSet, InputString) :-
   phrase(RuleSet, Input),
   string_codes(InputString, Input).
string_phrase(RuleSet, InputString, RestString) :-
   string_codes(InputString, Input),
   phrase(RuleSet, Input, RestCodes),
   string_codes(RestString, RestCodes).

:- meta_predicate
       phrase_file(//, +),
       phrase_file(//, +, ?).
phrase_file(RuleSet, File) :-
   open(File, read, Stream),
   read_string(Stream, _, String),
   string_phrase(RuleSet, String).
phrase_file(RuleSet, File, Rest) :-
   open(File, read, Stream),
   read_string(Stream, _, String),
   string_phrase(RuleSet, String, Rest).


:- meta_predicate lazy_sequence(:, :, ?, ?, ?).
lazy_sequence(_, _, []) --> [].
lazy_sequence(Element, _, [X]) --> call(Element, X).
lazy_sequence(Element, Sep, [X | Xs]) -->
    call(Element, X),
    Sep,
    lazy_sequence(Element, Sep, Xs).

chunks(Len, List, [Chunk | Chunks]) :-
   append(Chunk, Rest, List),
   length(Chunk, Len),
   chunks(Len, Rest, Chunks),
   !.
chunks(_, _, []).

% unify R with L, except replacing the value at position P (1-based) with E
replace1(L, P, E, R) :-
    PreLen is P - 1,
    length(Pre, PreLen),
    append(Pre, [_|T], L),
    append(Pre, [E|T], R).

replace0(List, Index, OldElem, NewElem, NewList) :-
   nth0(Index,List,OldElem,Transfer),
   nth0(Index,NewList,NewElem,Transfer).

list_product([], 1).
list_product([X | Xs], P) :-
   list_product(Xs, XsP),
   P #= XsP * X.

%%%

empty_set(Set) :- empty_assoc(Set).

set_add(Elem, Set0, Set1) :-
   put_assoc(Elem, Set0, true, Set1).

set_del(Elem, Set0, Set1) :-
   del_assoc(Elem, Set0, _, Set1).

set_member(Elem, Set) :-
   gen_assoc(Elem, Set, true).

set_memberchk(Elem, Set) :-
   get_assoc(Elem, Set, true).

list_to_set(List, Set) :-
   empty_set(Set0),
   foldl(set_add, List, Set0, Set).

set_to_list(Set, List) :-
   assoc_to_keys(Set, List).

set_cardinality(Set, Cardinality) :-
   set_to_list(Set, L),
   length(L, Cardinality).

number_base_decimal(N, Base, Dec) :-
   atomic_list_concat([Base, '\'', N], Atom),
   atom_to_term(Atom, Dec, _).

%%%

:- begin_tests(utils).

test(replace) :-
   replace1([a,b,c], 1, a2, [a2,b,c]),
   replace1([a,b,c], 2, b2, [a,b2,c]),
   replace1([a,b,c], 3, c2, [a,b,c2]),
   \+ replace1([a,b,c], 4, d2, _).

test(list_product) :-
   list_product([1234], 1234),
   list_product([1, 2, 3, 4], 24).

test(sets) :-
   empty_set(Set0),
   set_cardinality(Set0, 0),
   set_to_list(Set0, []),

   set_add(1, Set0, Set1),
   set_cardinality(Set1, 1),
   set_to_list(Set1, [1]),
   set_member(1, Set1),
   set_add(1, Set1, Set1),
   \+ set_member(2, Set1),

   set_add(2, Set1, Set2),
   set_member(2, Set2),
   set_del(2, Set2, Set1).

test(number_base_decimal) :-
   number_base_decimal("ff", 16, 255).

:- end_tests(utils).
