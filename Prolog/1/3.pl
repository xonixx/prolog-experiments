
f(1, 4) :-!.
f(N, Res) :-
	N1 is N - 1,
	f(N1, R1),
	g(N1, R2),
	Res is R1 + R2.

g(1, 2) :-!.
g(N, Res) :-
	N1 is N - 1,
	f(N1, R1),
	g(N1, R2),
	Res is R1 - R2.

main :-
	read(A),
	f(A, Res),
	write('f('),write(A),write(')='),write(Res).

