

simpl([x(A, B) | T], T3) :-
	(   select(x(A1, B), T, T1)
	->  A2 is A + A1,
	    simpl(T1, T2),
	    simpl([x(A2, B) | T2], T3)
	;   simpl(T, T4),
	    T3 = [x(A, B) | T4]
	).

simpl(L, L).
