a.

b :- fail.

f(1).
f(2) :-!.
f(3).

writeall :-
	f(A),
	write(A),nl,
	fail.

writeall :- write('All f(A) solutions found!').
