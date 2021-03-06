% L = [a-b-d-c, a-c-d-b, b-a-c-d, b-d-c-a, c-a-b-d, c-d-b-a, d-b-a-c, d-c-a-b].

% the_same(A-B-C-D, B-C-D-A) :-!.
% the_same(A-B-C-D, C-D-A-B) :-!.
% the_same(A-B-C-D, D-A-B-C) :-!.
% the_same(A-B-C-D, A-C-B-D) :-!.

the_same(A-B-C-D, E-F-G-H) :-
 sort([A, B, C, D], Res),
 sort([E, F, G, H], Res).

has(Squares, Sq) :-
 member(S, Squares),
 the_same(S, Sq).
 
wo_dublicates([], []).
wo_dublicates([H | T], Res) :-
 (has(T, H)
 ->
 wo_dublicates(T, Res)
 ;
 Res = [H | L],
 wo_dublicates(T, L)).

