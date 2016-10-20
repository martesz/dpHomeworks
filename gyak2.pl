:- use_module(library(lists)).
% Beszúrás rendezett listába
insert_ord([], X, [X]).
insert_ord([H | []], X, Result) :-
        (
           integer(X),
           X < H ->
           Result = [X,H]
        ;
           integer(X),
           X > H ->
           Result = [H,X]
        ;
           Result = [H]
        ).
insert_ord([H1, H2| T], X, Result) :-
        (
           H1 < X,
           H2 >= X ->
           insert_ord([H2|T], done, Result1),
           Result = [H1, X | Result1]
        ;
           insert_ord([H2|T], X, Result1),
           Result = [H1 | Result1]
        ).
           
select_edge(P, Q, G, G1) :-
        select(E, G, G1),
        (
           E = P-Q
        ;
           E = Q-P
        ).

draw(G, L) :-
        draw(G, _, L).
draw([], _, []).
draw(G, P, [P-Q|L]) :-
        select_edge(P, Q, G, G1),
        draw(G1, Q, L).

pl_kezdetu([Head, Head|T], H, M) :-
        pl_kezdetu_seged([Head|T], 2, H, M).
        
pl_kezdetu_seged([H1, H2|T], Count, H, M) :-
        (
           H1 == H2 ->
           Count1 is Count + 1,
           pl_kezdetu_seged([H2|T], Count1, H, M)
        ;
           M = [H2|T],
           H = Count
        ).
pl_kezdetu_seged([_H|[]], Count, Count, []).

plato(L, Len, X) :-
        (
           pl_kezdetu(L, Len1, M) ->
           (Len = Len1,
            [X|_T] = L
           ;
            plato(M, Len, X)
           )
        ;
           L = [_H|T],
           plato(T, Len, X)
        ).