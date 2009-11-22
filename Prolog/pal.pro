
rev([H | T], R) :-
	rev(T, TRev),
	append(TRev, [H], R).
rev([], []).
