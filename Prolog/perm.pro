
perm([], []).
perm([H | T], P) :-
	perm(T, Pt),
	append(A, B, Pt),
	append([A, [H], B], P).
