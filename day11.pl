% -*- mode: prolog -*-

:- use_module(utils).
:- use_module(library(readutil)).
:- use_module(library(dcg/high_order)).

%%%

line_edges(Line, Source - Targets) :-
    split_string(Line, ":", " ", [Source_s, Targets_string_with_space]),
    string_concat(" ", Targets_string_with_space, Targets_string),
    split_string(Targets_string, " ", "", Targets0),
    delete(Targets0, "", Targets_s),
    atom_string(Source, Source_s),
    maplist(atom_string, Targets, Targets_s).

read_input(File, Graph) :-
    open(File, read, Stream),
    read_lines(Stream, Lines),
    maplist(line_edges, Lines, Graph).

edge(Graph, V1, V2) :-
    member(V1 - Outgoing, Graph),
    member(V2, Outgoing).

path(Graph, V, V, [V]) :-
    ( member(V - _, Graph)
    ; member(_ - Outgoing, Graph), member(V, Outgoing)).
path(Graph, V1, V, [V1 | Path]) :-
    edge(Graph, V1, V2),
    path(Graph, V2, V, Path).

paths(Graph, V1, V2, Paths) :-
    setof(Path, path(Graph, V1, V2, Path), Paths).

solution_part1(Graph, Solution) :-
    aggregate_all(count, Path, path(Graph, you, out, Path), Solution).
