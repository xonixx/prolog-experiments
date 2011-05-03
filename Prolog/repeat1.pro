
:- dynamic e/1.

app(L1, L2, L) :-
	reverse(L1, L1Rev),
	assert(e(L2)),

	member(E, L1Rev),
	retract(e(L)),
	assert(e([E|L])),
	fail

	;

	retract(e(L)).

