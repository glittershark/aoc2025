% -*- mode: prolog -*-

:- use_module(utils).
:- use_module(library(readutil)).
:- use_module(library(assoc)).

%%%

read_input(File, Positions) :-
    open(File, read, Stream),
    read_lines(Stream, Lines),
    maplist([Line, pos(X, Y, Z)] >> (
                split_string(Line, ",", "", Num_strings),
                maplist(number_string, [X, Y, Z], Num_strings)
            ),
           Lines,
           Positions).

%%%

distance(pos(X1, Y1, Z1), pos(X2, Y2, Z2), Dist) :-
    Dist is sqrt((X1 - X2) ^ 2 + (Y1 - Y2) ^ 2 + (Z1 - Z2) ^ 2).

pairs(Positions, Pairs) :-
    setof(
        P1 - P2,
        (member(P1, Positions),
         member(P2, Positions),
         P1 @< P2),
        Pairs).

first_n_connections(N, Positions, Connections) :-
    pairs(Positions, Pairs),
    maplist(
        [Pair, Dist - Pair] >> (P1 - P2 = Pair, distance(P1, P2, Dist)),
        Pairs,
        Pair_dists),
    keysort(Pair_dists, Pair_dists_sorted),
    !,
    append(Conn_dists, _, Pair_dists_sorted),
    length(Conn_dists, N),
    pairs_values(Conn_dists, Connections).


% sccs(Sccs, By_point)
%
% Sccs is an assoc from Scc_id to a set of points (assoc from point to true)
% By_point is an asssoc from point to Scc_id

sccs([], Acc, Acc).
sccs([Point1 - Point2 | Connections], sccs(Sccs, By_point), Res) :-
    ( get_assoc(Point1, By_point, Scc_id1) ->
      % Point1 is in an scc
      get_assoc(Scc_id1, Sccs, Scc1),
      ( get_assoc(Point2, By_point, Scc_id2), Scc_id1 \= Scc_id2 ->
        % Both are in different sccs, merge (scc2 into scc1)
        get_assoc(Scc_id2, Sccs, Scc2),
        assoc_to_keys(Scc2, Scc2_points),
        foldl(
            {Scc_id1} / [ Point,
                          Scc_prev - By_point_prev,
                          Scc_next - By_point_next
                        ] >> (
                put_assoc(Point, Scc_prev, true, Scc_next),
                put_assoc(Point, By_point_prev, Scc_id1, By_point_next)
            ),
            Scc2_points,
            Scc1 - By_point,
            Scc1_next - By_point_next),
        del_assoc(Scc_id2, Sccs, _, Sccs_next1),
        put_assoc(Scc_id1, Sccs_next1, Scc1_next, Sccs_next)
      ; % Otherwise, add point2 to Scc1
        put_assoc(Point2, Scc1, true, Scc1_next),
        put_assoc(Scc_id1, Sccs, Scc1_next, Sccs_next),
        put_assoc(Point2, By_point, Scc_id1, By_point_next)
      )
    ; get_assoc(Point2, By_point, Scc_id2) ->
      % Point2 is in an scc (and point 1 isn't), add point 1 to scc2
      get_assoc(Scc_id2, Sccs, Scc2),
      put_assoc(Point1, Scc2, true, Scc2_next),
      put_assoc(Scc_id2, Sccs, Scc2_next, Sccs_next),
      put_assoc(Point1, By_point, Scc_id2, By_point_next)
    ; % Neither is in an scc, make a new scc and put them both in it
      ( empty_assoc(Sccs) -> Scc_id = 0
      ; max_assoc(Sccs, Max_id, _),
        Scc_id is Max_id + 1
      ),
      ( Point1 = Point2 ->
        list_to_assoc([Point1 - true], Scc)
      ; list_to_assoc([ Point1 - true, Point2 - true ], Scc)
      ),
      put_assoc(Scc_id, Sccs, Scc, Sccs_next),
      put_assoc(Point1, By_point, Scc_id, By_point_next1),
      put_assoc(Point2, By_point_next1, Scc_id, By_point_next)
    ),
    sccs(Connections, sccs(Sccs_next, By_point_next), Res).
sccs(Connections, Res) :-
    Acc = sccs(Sccs, By_point),
    empty_assoc(Sccs),
    empty_assoc(By_point),
    sccs(Connections, Acc, Res).

scc_size(Scc, Size) :-
    assoc_to_keys(Scc, L),
    length(L, Size).


solution_part1(N_conns, Positions, Solution) :-
    first_n_connections(N_conns, Positions, Conns),
    sccs(Conns, sccs(Sccs, _)),
    assoc_to_values(Sccs, Sccs_list),
    maplist(scc_size, Sccs_list, Sizes),
    msort(Sizes, Sizes_sorted),
    append(_, Prefix, Sizes_sorted),
    length(Prefix, 3),
    list_product(Prefix, Solution).

solution_part1(Positions, Solution) :-
    solution_part1(1000, Positions, Solution).

%%%
%%% part 2
%%%

complete_circuit([Pair | Pairs], Sccs, Conns, Res) :-
    sccs([Pair], Sccs, Sccs1),
    sccs(Components, _) = Sccs1,
    ( assoc_to_keys(Components, [_]) ->
      Res = [Pair | Conns]
    ;
    complete_circuit(Pairs, Sccs1, [Pair | Conns], Res)
    ).

complete_circuit(Positions, Conns) :-
    maplist([P, P - P] >> true, Positions, Self_conns),
    sccs(Self_conns, Init_sccs),
    pairs(Positions, Pairs),
    maplist(
        [Pair, Dist - Pair] >> (P1 - P2 = Pair, distance(P1, P2, Dist)),
        Pairs,
        Pair_dists),
    keysort(Pair_dists, Pair_dists_sorted),
    pairs_values(Pair_dists_sorted, Pairs_sorted),
    !,
    complete_circuit(Pairs_sorted, Init_sccs, [], Conns).

solution_part2(Positions, Solution) :-
    complete_circuit(Positions, [Last_conn | _]),
    pos(X1, _, _) - pos(X2, _, _) = Last_conn,
    Solution is X1 * X2.
