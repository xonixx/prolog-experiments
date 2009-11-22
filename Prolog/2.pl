
elems(C, Res) :-
	(   C = A + B; C = A * B; C = A / B; C = A - B),
	elems(A, ARes),
	elems(B, BRes),
	append(ARes, BRes, Res), !.
elems(C, [C]).
