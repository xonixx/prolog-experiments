

break(A0,(A1, A2)) :-
	append(A1, A2, A0),
	length(A1, L1),
	length(A2, L2),
	D is L2 - L1,
	(   D = 0; D = 1),
	!.

binsearch(_, []) :- fail, !.
binsearch(El, [El]) :-!.
binsearch(El, L) :-
	break(L, (L1, [H|L2])),
	(   H > El
	->  binsearch(El, L1)
	;   H < El
	->  binsearch(El, L2)
	;   true
	).

