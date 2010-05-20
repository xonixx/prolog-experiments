
:- use_module(library(clpfd)).

pLeap(L,H,X,Y) :-
	X in L..H,
        Y #= min(H, X+1),
	label([X]).
