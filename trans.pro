tr(L,L1) :-
	append([H | B], [T],L), !,
	append([T | B], [H],L1).
