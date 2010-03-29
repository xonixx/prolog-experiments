:-use_module(library(clpfd)).

solve(GG) :-
	GG = [
	      [A,B,C,D],
	      [E,F,G,H],
	      [I,J,K,L],
	      [M,N,O,P]
	    ],
	append(GG, Vars),
	Vars ins 0..1,

	sum(Vars,#=,10),

	(   A + B + C + D) mod 2 #= 0,
	(   E + F + G + H) mod 2 #= 0,
	(   I + J + K + L) mod 2 #= 0,
	(   M + N + O + P) mod 2 #= 0,

	(   A + E + I + M) mod 2 #= 0,
	(   B + F + J + N) mod 2 #= 0,
	(   C + G + K + O) mod 2 #= 0,
	(   D + H + L + P) mod 2 #= 0,

	label(Vars).

solve :- forall(solve(GG),format('~n~w~n~w~n~w~n~w~n-------',GG)).
