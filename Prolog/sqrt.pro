
/*
sqrt(X, Dmin, R0, R) :-
	R1 is (R0 + X / R0) / 2,
	D is abs(X - R1*R1),
	(   D > Dmin
	->  sqrt(X, Dmin, R1, R)
	;   R = R0
	).
*/

sqrt(X, Dmin, R, R) :- abs(X - R * R) < Dmin, !.
sqrt(X, Dmin, R0, R) :-
	R1 is (R0 + X / R0) / 2,
	format('Trying: ~w~n', [R1]),
	sqrt(X, Dmin, R1, R).

sqrt(X, R) :-
	sqrt(X, 0.001, 1, R).

