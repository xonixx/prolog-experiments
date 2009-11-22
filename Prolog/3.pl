
double([], []) :-!.
double([H | T], [H2 | T2]) :-
	H2 is H * 2,
	double(T, T2).
