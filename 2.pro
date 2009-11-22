


relation(1,m).
relation(2,w).
relation(3,m).
relation(4,m).
relation(5,w).

:- op(500, xfx, :=).
:- dynamic [(:=)/2].


how_much_w(R) :-
	assert(n := 0),
	calc_how_much_w,
	retract(n := R).


calc_how_much_w :-
	relation(_,w),
	retract(n := N),
	N1 is N + 1,
	assert(n := N1),
	fail;
	true.
