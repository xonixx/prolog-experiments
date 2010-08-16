
cartesian([S | SS], [H | T]) :-
	member(H, S),
	(   SS = []
	->  T = []
	;   cartesian(SS, T)
	).


